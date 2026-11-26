#lang racket/base

;; Minimal user-facing participant/topic API: the glue that turns the
;; wire-level layers (SPDP discovery, SEDP endpoint discovery,
;; reliable machines, CDR type codecs) into a pub/sub surface.
;;
;; Shape (one participant = one endpoint + one SPDP writer/reader +
;; one SEDP domain):
;;   (define p (make-participant))
;;   (define t (participant-topic! p "Speed" speed-type
;;               #:reliability 'reliable #:durability 'transient-local))
;;   (define w (topic-writer! t))
;;   (writer-write! w '((kph . 42)))
;;   (topic-reader! t (lambda (v sn) ...))
;;
;; Design notes (honest scope for a tooling-grade stack):
;;   - ONE reliable machine per topic writer, shared by all remote
;;     readers: nack retransmission is idempotent and the final-ack
;;     prune fires on any acking reader. Per-reader state machines
;;     (the full DDS StatefulWriter) remain a refinement.
;;   - DATA fan-out goes unicast to every participant announcing a
;;     matching reader endpoint; target addresses come from the
;;     participant's metatraffic locator (SPDP view).
;;   - Discovery matching is on topic name; QoS compatibility is not
;;     negotiated (the tooling use case controls both ends).
;;   - Best-effort topics skip the reliable machines: DATA is sent
;;     as-is and delivered on arrival.
;;   - Late joiners of a transient-local topic receive the writer's
;;     retained history (reliable-writer-resend-history!), so
;;     keep-last depth IS the durability window.

(require racket/contract
         racket/match
         "cdr.rkt"
         "entities.rkt"
         "message.rkt"
         "reliable.rkt"
         "sedp.rkt"
         "spdp.rkt"
         "topic-type.rkt"
         "transport.rkt"
         "logging.rkt")

(provide (struct-out participant)
         (struct-out topic)
         (struct-out writer-handle)
         make-participant
         participant-close!
         participant-topic!
         topic-writer!
         topic-reader!
         writer-write!)

(module+ test (require rackunit racket/list))

;; ------------------------------------------------------------------
;; Structures

(struct participant
  (endpoint        ; rtps-endpoint? (owned)
   prefix          ; our guid prefix
   spdp-w          ; spdp-writer?
   spdp-r          ; spdp-reader?
   sedp            ; sedp-domain?
   topics          ; hash: topic-name -> topic
   peers           ; hash: prefix-bytes -> participant-data (SPDP view)
   follow          ; hash: (cons prefix writer-entity) -> follow-entry
   writers         ; hash: writer-entity-bytes -> writer-handle
   entity-counter  ; box: unique user endpoint ids WITHIN this participant
   closed?)
  #:mutable)

(struct topic
  (name
   type            ; dds-type?
   reliability     ; 'reliable or 'best-effort
   durability      ; 'volatile or 'transient-local
   history-depth   ; keep-last depth or #f
   writer-handle   ; writer-handle? or #f
   reader-callback) ; (-> value sn any/c) or #f
  #:mutable)

(struct writer-handle
  (topic
   entity-id       ; our user writer entity id (4 octets)
   machine         ; reliable-writer? for reliable topics, #f best-effort
   besn            ; box: best-effort sequence counter
   targets         ; hash: peer-prefix -> (cons host port) data targets
   owner)          ; participant? (the declaring participant)
  #:mutable)

;; follow entries: (list 'reliable reliable-reader? topic) for a
;; remote writer we follow, (list 'best-effort #f topic) otherwise
(define (follow-entry? v)
  (and (list? v) (= 3 (length v)) (memq (car v) '(reliable best-effort))))

;; ------------------------------------------------------------------
;; Participant lifecycle

;; Build a participant. The endpoint binds `bind-port` (default: the
;; well-known discovery unicast port 7401) and joins
;; `multicast-groups` (default: none, sandbox-safe; pass
;; (list default-discovery-multicast-address) for vendor interop).
;; SPDP announcements go to the discovery multicast address plus
;; every unicast target learned from remote announcements.
(define (make-participant
         #:domain-id [domain-id 0]
         #:bind-port [bind-port #f]
         #:multicast-groups [multicast-groups '()]
         #:guid-prefix [prefix #f])
  (define the-prefix (or prefix (random-guid-prefix)))
  (define p-placeholder #f)

  ;; bind first (0 picks an ephemeral port), then advertise the actual
  ;; bound port in the locators
  (define ep (make-rtps-endpoint #:bind-port (or bind-port 0)
                                 #:multicast-groups multicast-groups
                                 #:handler void))
  (define port (endpoint-udp-port ep))

  (define topics (make-hash))
  (define peers (make-hash))
  (define follow (make-hash))
  (define writers (make-hash))

  (define-values (sedp-domain sedp-handler)
    (make-sedp-domain
     #:guid-prefix the-prefix
     #:send!
     (lambda (bs)
       ;; unicast to every known participant's metatraffic locator
       (for ([(pkey rec) (in-hash peers)])
         (define met (participant-data-metatraffic-unicast rec))
         (when (and met (locator-udpv4? met))
           (endpoint-send! ep (locator-ip met) (locator-port met) bs))))
     #:header! (lambda () (make-rtps-header the-prefix))
     #:event! (lambda (event ep-disc)
                (route-sedp-event! p-placeholder event ep-disc))))

  (define-values (spdp-r spdp-handler)
    (make-spdp-reader
     #:self-prefix the-prefix
     #:event!
     (lambda (event record reason)
       (define pkey (guid-prefix (participant-data-guid record)))
       (cond
         [(eq? event 'discovered)
          (hash-set! peers pkey record)
          (define met (participant-data-metatraffic-unicast record))
          (when (and met (locator-udpv4? met))
            (spdp-writer-add-target! spdp-w (locator-ip met) (locator-port met)))
          (sedp-participant-seen! sedp-domain record)]
         [(eq? event 'removed)
          (hash-remove! peers pkey)
          (sedp-participant-gone! sedp-domain record)]
         [(eq? event 'updated)
          (hash-set! peers pkey record)]))))

  (define spdp-w
    (make-spdp-writer
     #:send!
     (lambda (bs)
       (for ([t (unbox (spdp-writer-targets spdp-w))])
         (endpoint-send! ep (car t) (cadr t) bs))
       (with-handlers ([exn:fail? (lambda (e)
                                    (dds-log-debug
                                     "participant: multicast announce skipped: ~a"
                                     (exn-message e)))])
         (endpoint-send-multicast! ep default-discovery-multicast-address
                                   (discovery-multicast-port domain-id) bs)))
     #:send-target! (lambda (host port bs) (endpoint-send! ep host port bs))
     #:guid-prefix the-prefix
     #:domain-id domain-id
     #:metatraffic-unicast (udpv4-locator "127.0.0.1" port)
     #:default-unicast (udpv4-locator "127.0.0.1" port)
     #:metatraffic-multicast
     (udpv4-locator default-discovery-multicast-address
                    (discovery-multicast-port domain-id))))

  (set-rtps-endpoint-handler!
   ep (lambda (header subs sender)
        (spdp-handler header subs sender)
        (sedp-handler header subs sender)
        (route-user-data! p-placeholder header subs sender)))

  (define p
    (participant ep the-prefix spdp-w spdp-r sedp-domain
                 topics peers follow writers (box 1) #f))
  (set! p-placeholder p)
  p)

(define (participant-open? p) (not (participant-closed? p)))

(define (participant-close! p)
  (unless (participant-closed? p)
    (set-participant-closed?! p #t)
    (spdp-writer-dispose! (participant-spdp-w p))
    (spdp-reader-stop! (participant-spdp-r p))
    (for ([(name t) (in-hash (participant-topics p))])
      (define wh (topic-writer-handle t))
      (when (and wh (writer-handle-machine wh))
        (reliable-writer-close! (writer-handle-machine wh))))
    (endpoint-close! (participant-endpoint p))))

;; ------------------------------------------------------------------
;; Topics

;; unique user endpoint entity id within the participant: counter +
;; kind octet (0x03 user writer without key, 0x07 user reader without
;; key). DDS entity ids are unique per participant, NOT per topic -
;; the SEDP table keys endpoints by guid bytes, so a per-topic counter
;; would make two topics collide and overwrite each other.
(define (next-entity-id! p kind)
  (define c (unbox (participant-entity-counter p)))
  (set-box! (participant-entity-counter p) (add1 c))
  (bytes (bitwise-and (arithmetic-shift c -8) #xFF)
         (bitwise-and c #xFF)
         0
         kind))

;; Declare a topic on the participant: registers the type/QoS bundle
;; and announces writer AND reader endpoints through SEDP (so the peer
;; matches whichever role it has). Returns the topic handle.
(define (participant-topic! p name type
                            #:reliability [reliability 'reliable]
                            #:durability [durability 'volatile]
                            #:history-depth [history-depth #f])
  (unless (participant-open? p)
    (raise-argument-error 'participant-topic! "open participant" p))
  (unless (string? name) (raise-argument-error 'participant-topic! "string topic name" name))
  (unless (dds-type? type) (raise-argument-error 'participant-topic! "dds-type?" type))
  (unless (memq reliability '(reliable best-effort))
    (raise-argument-error 'participant-topic! "'reliable/'best-effort" reliability))
  (unless (memq durability '(volatile transient-local))
    (raise-argument-error 'participant-topic! "'volatile/'transient-local" durability))
  (when (hash-has-key? (participant-topics p) name)
    (raise-argument-error 'participant-topic! "topic name not already declared" name))
  (when (and history-depth (not (exact-positive-integer? history-depth)))
    (raise-argument-error 'participant-topic! "exact positive depth or #f" history-depth))
  (define t
    (topic name type reliability durability history-depth
           #f #f))
  (hash-set! (participant-topics p) name t)
  (declare-writer-role! p t)
  (declare-reader-role! p t)
  ;; match against remote endpoints ALREADY known to SEDP: discovery
  ;; data that arrived before this topic existed was dropped by the
  ;; event router, so a late topic declaration must sweep the table
  ;; (the DDS model matches on both endpoint creations)
  (for ([(guid-bytes remote-ep) (in-hash (sedp-domain-endpoints (participant-sedp p)))]
        #:when (string=? (discovered-endpoint-topic remote-ep) name))
    (cond
      [(eq? (discovered-endpoint-kind remote-ep) 'writer)
       (attach-remote-writer! p t
                              (guid-prefix (discovered-endpoint-guid remote-ep))
                              (guid-entity (discovered-endpoint-guid remote-ep)))]
      [(eq? (discovered-endpoint-kind remote-ep) 'reader)
       (attach-remote-reader! p t
                              (guid-prefix (discovered-endpoint-guid remote-ep)))]))
  t)

(define (declare-writer-role! p t)
  (define entity (next-entity-id! p #x03))
  (define machine
    (if (eq? (topic-reliability t) 'reliable)
        (make-reliable-writer
         #:writer-id entity
         #:writer-prefix (participant-prefix p)
         #:send! (lambda (bs) (fan-out! p t bs))
         #:make-header (lambda () (make-rtps-header (participant-prefix p)))
         #:durability (topic-durability t)
         #:history-depth (topic-history-depth t)
         ;; periodic heartbeats converge the reader/late-join races
         ;; (a history push may outrun the peer's reader machine
         ;; creation; the next heartbeat recovers via nack)
         #:heartbeat-period 1.0)
        #f))
  (define wh (writer-handle t entity machine (box 1) (make-hash) p))
  (set-topic-writer-handle! t wh)
  (hash-set! (participant-writers p) entity wh)
  (sedp-announce-writer! (participant-sedp p)
                         (discovered-endpoint
                          (make-guid (participant-prefix p) entity)
                          'writer (topic-name t)
                          (dds-type-name (topic-type t))
                          (topic-reliability t)
                          (if (eq? (topic-durability t) 'transient-local)
                              'transient-local 'volatile)
                          #f)))

(define (declare-reader-role! p t)
  (define entity (next-entity-id! p #x07))
  (sedp-announce-reader! (participant-sedp p)
                         (discovered-endpoint
                          (make-guid (participant-prefix p) entity)
                          'reader (topic-name t)
                          (dds-type-name (topic-type t))
                          (topic-reliability t)
                          (if (eq? (topic-durability t) 'transient-local)
                              'transient-local 'volatile)
                          #f)))

;; ------------------------------------------------------------------
;; Discovery wiring

;; SEDP discovery events drive the follow tables:
;;   remote writer for a topic we have -> reader-side machine
;;     (reliable) or a best-effort follow marker
;;   remote reader for a topic we have -> remember the peer as a data
;;     target; for reliable topics push our retained history (the
;;     transient-local late-join mechanism)
;;   removed -> drop the follow entry
(define (route-sedp-event! p event ep)
  (when p
    (define t (hash-ref (participant-topics p) (discovered-endpoint-topic ep) #f))
    (unless t
      (dds-log-debug "topic: discovery for unknown topic ~a ignored"
                     (discovered-endpoint-topic ep)))
    (when t
      (define peer-prefix (guid-prefix (discovered-endpoint-guid ep)))
      (define entity (guid-entity (discovered-endpoint-guid ep)))
      (cond
        [(eq? event 'writer-discovered)
         (attach-remote-writer! p t peer-prefix entity)]
        [(eq? event 'reader-discovered)
         (attach-remote-reader! p t peer-prefix)]
        [(eq? event 'removed)
         (hash-remove! (participant-follow p) (cons peer-prefix entity))]))))

(define (attach-remote-writer! p t peer-prefix entity)
  (define entry
    (if (eq? (topic-reliability t) 'reliable)
        (list 'reliable
              (make-reliable-reader
               #:reader-id (next-entity-id! p #x07)
               #:writer-prefix peer-prefix
               #:writer-id entity
               #:deliver! (lambda (sn payload qos)
                            (deliver-sample! p t sn payload))
               #:send! (lambda (bs) (send-to-peer! p peer-prefix bs))
               #:make-header (lambda () (make-rtps-header (participant-prefix p))))
              t)
        (list 'best-effort #f t)))
  (hash-set! (participant-follow p) (cons peer-prefix entity) entry))

(define (attach-remote-reader! p t peer-prefix)
  (define wh (topic-writer-handle t))
  (when wh
    (define target (peer-data-address p peer-prefix))
    (when target
      (hash-set! (writer-handle-targets wh) peer-prefix target))
    ;; push our retained history: this IS the transient-local
    ;; late-join mechanism; for volatile topics the history is empty
    ;; once acked, so this is a no-op plus a heartbeat
    (when (writer-handle-machine wh)
      (reliable-writer-resend-history! (writer-handle-machine wh)))))

(define (peer-data-address p peer-prefix)
  (define rec (hash-ref (participant-peers p) peer-prefix #f))
  (and rec
       (let ([loc (or (participant-data-metatraffic-unicast rec)
                      (participant-data-default-unicast rec))])
         (and loc (locator-udpv4? loc)
              (cons (locator-ip loc) (locator-port loc))))))

(define (send-to-peer! p peer-prefix bs)
  (define target (peer-data-address p peer-prefix))
  (when target
    (endpoint-send! (participant-endpoint p) (car target) (cdr target) bs)))

;; fan a topic's messages out to every participant with a matching
;; reader (reliable machines produce their own DATA + heartbeats
;; through the send! that lands here)
(define (fan-out! p t bs)
  (define wh (topic-writer-handle t))
  (define targets (if wh (hash-values (writer-handle-targets wh)) '()))
  (for ([tgt targets])
    (endpoint-send! (participant-endpoint p) (car tgt) (cdr tgt) bs)))

;; ------------------------------------------------------------------
;; User data routing on the endpoint

(define (route-user-data! p header subs sender)
  (when p
    (define src-prefix (rtps-header-guid-prefix header))
    (for ([sub subs])
      (cond
        [(data-sub? sub)
         (define entry (hash-ref (participant-follow p)
                                 (cons src-prefix (data-sub-writer-id sub)) #f))
         (when entry
           (case (car entry)
             [(reliable) (reliable-reader-on-data! (cadr entry) sub)]
             [(best-effort)
              (deliver-sample! p (caddr entry) (data-sub-sn sub)
                               (data-sub-payload sub))]))]
        [(heartbeat-sub? sub)
         (define entry (hash-ref (participant-follow p)
                                 (cons src-prefix (heartbeat-sub-writer-id sub)) #f))
         (when (and entry (eq? (car entry) 'reliable))
           (reliable-reader-on-heartbeat! (cadr entry) sub))]
        [(acknack-sub? sub)
         (define wh (hash-ref (participant-writers p)
                              (acknack-sub-writer-id sub) #f))
         (when (and wh (writer-handle-machine wh))
           (reliable-writer-on-acknack! (writer-handle-machine wh) sub))]
        [(and (list? sub) (= 5 (length sub)) (eq? 'gap (car sub)))
         (define entry (hash-ref (participant-follow p)
                                 (cons src-prefix (caddr sub)) #f))
         (when (and entry (eq? (car entry) 'reliable))
           (reliable-reader-on-gap! (cadr entry) (cadddr sub) (car (cddddr sub))))]))))

;; decode a payload and hand it to the topic callback
(define (deliver-sample! p t sn payload)
  (define callback (topic-reader-callback t))
  (when (and callback payload (not (equal? payload #"")))
    (let-values ([(v _scheme) (type-decode (topic-type t) payload)])
      (callback v sn))))

;; ------------------------------------------------------------------
;; Public write/read surface

;; Create (or return the existing) writer for a topic.
(define (topic-writer! t)
  (or (topic-writer-handle t)
      (raise-argument-error 'topic-writer!
                            "topic with a writer (declared via participant-topic!)"
                            t)))

;; Register a sample callback for a topic. Returns the topic.
(define (topic-reader! t callback)
  (unless (procedure? callback)
    (raise-argument-error 'topic-reader! "procedure?" callback))
  (set-topic-reader-callback! t callback)
  t)

;; Publish one sample: encode through the topic type, then hand it to
;; the reliable machine (which emits DATA + heartbeat) or straight to
;; the wire for best-effort topics. Returns the sequence number.
(define (writer-write! wh v)
  (define p (writer-handle-owner wh))
  (unless (participant-open? p)
    (raise-argument-error 'writer-write! "open participant" wh))
  (define t (writer-handle-topic wh))
  (define payload (type-encode (topic-type t) v))
  (define machine (writer-handle-machine wh))
  (if machine
      (reliable-writer-write! machine payload)
      (let ()
        (define sn (unbox (writer-handle-besn wh)))
        (set-box! (writer-handle-besn wh) (add1 sn))
        (define msg
          (encode-rtps-message
           (make-rtps-header (participant-prefix p))
           (list (data-sub entity-id-unknown
                           (writer-handle-entity-id wh)
                           sn '() payload #f))))
        (fan-out! p t msg)
        sn)))

;; ------------------------------------------------------------------
;; Tests: two participants over the in-memory wire (same trick as
;; sedp.rkt), covering discovery match, reliable exchange, QoS and
;; best-effort topics

(module+ test
  (define speed-type
    (struct-t (struct-member 'kph u16-t)
              (struct-member 'label string-t)))

  ;; bounded wait helper for asynchronous loopback convergence
  (define (wait-for proc timeout-secs)
    (let loop ([deadline (+ (current-inexact-milliseconds)
                            (* 1000 timeout-secs))])
      (or (proc)
          (and (< (current-inexact-milliseconds) deadline)
               (sleep 0.05)
               (loop deadline)))))

  (define p-a (make-participant #:bind-port 0))
  (define p-b (make-participant #:bind-port 0))

  ;; discover each other via real UDP loopback on 127.0.0.1
  (spdp-writer-add-target! (participant-spdp-w p-a) "127.0.0.1"
                           (endpoint-udp-port (participant-endpoint p-b)))
  (spdp-writer-add-target! (participant-spdp-w p-b) "127.0.0.1"
                           (endpoint-udp-port (participant-endpoint p-a)))
  (spdp-writer-announce! (participant-spdp-w p-a))
  (spdp-writer-announce! (participant-spdp-w p-b))
  (check-true (wait-for (lambda ()
                          (and (positive? (hash-count (participant-peers p-a)))
                               (positive? (hash-count (participant-peers p-b)))))
                        5)
              "participants discover each other")

  ;; both sides declare the same topic: A writes, B reads
  (define t-a (participant-topic! p-a "Speed" speed-type
                                  #:durability 'transient-local
                                  #:history-depth 5))
  (define t-b (participant-topic! p-b "Speed" speed-type
                                  #:durability 'transient-local
                                  #:history-depth 5))
  (define got (box '()))
  (topic-reader! t-b (lambda (v sn)
                       (set-box! got (cons (cons sn v) (unbox got)))))

  ;; SEDP exchange is asynchronous over loopback: wait until A's
  ;; writer has B as a data target (the match has converged)
  (check-true (wait-for (lambda ()
                          (not (null? (hash-keys
                                       (writer-handle-targets
                                        (topic-writer-handle t-a))))))
                        5)
              "SEDP match converged (A's writer sees B's reader)")

  (check-equal? (writer-write! (topic-writer! t-a) '((kph . 42) (label . "a"))) 1)
  (check-equal? (writer-write! (topic-writer! t-a) '((kph . 43) (label . "b"))) 2)
  (check-true (wait-for (lambda () (>= (length (unbox got)) 2)) 5)
              "both samples delivered in order")
  (check-equal? (reverse (unbox got))
                (list (cons 1 '((kph . 42) (label . "a")))
                      (cons 2 '((kph . 43) (label . "b")))))

  ;; best-effort topic in the other direction
  (define raw-type (struct-t (struct-member 'ticks u32-t)))
  (define t-c (participant-topic! p-b "Ticks" raw-type #:reliability 'best-effort))
  (define be-got (box '()))
  (topic-reader! (participant-topic! p-a "Ticks" raw-type #:reliability 'best-effort)
                 (lambda (v sn) (set-box! be-got (cons v (unbox be-got)))))
  ;; best-effort has no retransmission: wait for the match to converge
  ;; before the single write
  (check-true (wait-for (lambda ()
                          (not (null? (hash-keys
                                       (writer-handle-targets
                                        (topic-writer-handle t-c))))))
                        5)
              "best-effort match converged")
  (writer-write! (topic-writer! t-c) '((ticks . 7)))
  (check-true (wait-for (lambda ()
                          (and (member '((ticks . 7)) (unbox be-got)) #t))
                        5)
              "best-effort sample crosses the loopback")

  ;; late join: a third participant subscribes after the samples were
  ;; written; transient-local history must deliver them
  (define p-c (make-participant #:bind-port 0))
  (spdp-writer-add-target! (participant-spdp-w p-a) "127.0.0.1"
                           (endpoint-udp-port (participant-endpoint p-c)))
  (spdp-writer-add-target! (participant-spdp-w p-c) "127.0.0.1"
                           (endpoint-udp-port (participant-endpoint p-a)))
  (spdp-writer-announce! (participant-spdp-w p-a))
  (spdp-writer-announce! (participant-spdp-w p-c))
  (check-true (wait-for (lambda () (positive? (hash-count (participant-peers p-c)))) 5)
              "late joiner discovers A")
  (define late-got (box '()))
  (topic-reader! (participant-topic! p-c "Speed" speed-type
                                    #:durability 'transient-local
                                    #:history-depth 5)
                 (lambda (v sn) (set-box! late-got (cons (cons sn v) (unbox late-got)))))
  (check-true (wait-for (lambda () (>= (length (unbox late-got)) 2)) 5)
              "late joiner receives the transient-local history")
  (check-equal? (reverse (map car (unbox late-got))) '(1 2)
                "late joiner history arrives in order")

  (participant-close! p-a)
  (participant-close! p-b)
  (participant-close! p-c))
