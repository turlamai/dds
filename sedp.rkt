#lang racket/base

;; SEDP - Simple Endpoint Discovery Protocol (DDS-RTPS 2.3, chapter
;; 8.5.4 "Simple Endpoint Discovery Protocol").
;;
;; SEDP runs over two built-in RELIABLE endpoint pairs:
;;   publications: writer ENTITYID_SEDP_BUILTIN_PUBLICATIONS_WRITER
;;                 (0x000300|0x02) / reader ..._READER (0x000300|0x07)
;;   subscriptions: 0x000400|0x02 / 0x000400|0x07
;; Each participant announces its user endpoints (writers and readers)
;; through these pairs to every participant it has discovered via
;; SPDP [8.5.4.1].
;;
;; Wire rules implemented here:
;;   [8.5.4.2] DISCOVERED_WRITER_DATA / DISCOVERED_READER_DATA are
;;             parameter lists carrying at least PID_ENDPOINT_GUID,
;;             PID_TOPIC_NAME, PID_RELIABILITY; type name and endpoint
;;             locators ride along when known
;;   [8.2.5]   endpoint disposal travels as STATUS_INFO disposed in
;;             the inlineQos, keyed by PID_KEY_HASH, no payload
;;   [8.4.2]   the exchange is reliable: heartbeats drive acknacks,
;;             nacked samples are retransmitted by the writer side
;;
;; The domain is transport-agnostic: `send!` routes an encoded message
;; (typically: UDP unicast to every known participant's metatraffic
;; locator), and the returned `handler` plugs into the endpoint next
;; to the SPDP handler.

(require racket/contract
         racket/match
         "cdr.rkt"
         "entities.rkt"
         "message.rkt"
         "reliable.rkt"
         "spdp.rkt"
         "logging.rkt")

(provide (struct-out discovered-endpoint)
         (struct-out sedp-domain)
         make-sedp-domain
         sedp-participant-seen!
         sedp-participant-gone!
         sedp-announce-writer!
         sedp-announce-reader!
         sedp-dispose-endpoint!
         endpoint-announcement-params
         parse-endpoint-params)

(module+ test (require rackunit racket/list racket/set "transport.rkt"))

;; ------------------------------------------------------------------
;; Endpoint descriptions

(struct discovered-endpoint
  (guid              ; endpoint guid
   kind              ; 'writer or 'reader
   topic             ; string
   type              ; string or #f
   reliability       ; 'best-effort or 'reliable
   durability        ; 'volatile or 'transient-local or #f
   unicast-locator)  ; locator? or #f
  #:transparent)

;; Endpoint announcement parameter list [8.5.4.2]
(define (endpoint-announcement-params ep)
  (append
   (list
    (param-endpoint-guid (discovered-endpoint-guid ep))
    (param-string pid-topic-name (discovered-endpoint-topic ep))
    (param-reliability (discovered-endpoint-reliability ep)))
   (let ([type (discovered-endpoint-type ep)])
     (if type (list (param-string pid-type-name type)) '()))
   (let ([loc (discovered-endpoint-unicast-locator ep)])
     (if (locator? loc) (list (param-locator pid-unicast-locator loc)) '()))
   (if (discovered-endpoint-durability ep)
       (list (param-durability (discovered-endpoint-durability ep)))
       '())))

;; Parse an endpoint announcement; the reliability code is mapped to a
;; symbol, unknown codes pass through as raw numbers.
(define (parse-endpoint-params params le?)
  (define (find pid)
    (findf (lambda (p) (= pid (param-pid p))) params))
  (define guid-param (find pid-endpoint-guid))
  (unless guid-param
    (raise (exn:fail:dds:malformed
            "dds-sedp: endpoint announcement without PID_ENDPOINT_GUID"
            (current-continuation-marks))))
  (define topic-param (find pid-topic-name))
  (define rel-param (find pid-reliability))
  (define-values (rel-code _blocking) (if rel-param (param->reliability rel-param le?) (values 0 #f)))
  (define dur-param (find pid-durability))
  (discovered-endpoint
   (param->guid guid-param)
   #f ; kind filled by the caller (which builtin endpoint delivered it)
   (if topic-param (param->string topic-param) "")
   (let ([tp (find pid-type-name)])
     (and tp (param->string tp)))
   (case rel-code
     [(2) 'reliable]
     [(1) 'best-effort]
     [else 'unknown])
   (let ([dp dur-param])
     (and dp
          (case (param->durability dp le?)
            [(2) 'transient-local]
            [(1) 'volatile]
            [else 'unknown])))
   (let ([lp (find pid-unicast-locator)])
     (and lp (bytes->locator (param-value lp))))))

;; ------------------------------------------------------------------
;; SEDP domain

(struct sedp-domain
  (prefix          ; our participant guid prefix
   header!         ; (-> rtps-header?)
   send!           ; (-> bytes? any/c) route to all known participants
   writers         ; hash: 'publications / 'subscriptions -> reliable-writer
   readers         ; hash: (cons prefix-bytes writer-id-bytes) -> reliable-reader
   endpoints       ; hash: endpoint-guid-bytes -> discovered-endpoint
   participants    ; hash: prefix-bytes -> #t (metatraffic targets known)
   locals          ; hash: endpoint-guid-bytes -> discovered-endpoint (announced)
   event!          ; (-> symbol? discovered-endpoint? any/c)
   closed?)
  #:mutable)

;; Build an SEDP domain. `send!` routes encoded messages (unicast to
;; every participant's metatraffic locator); `header!` supplies our
;; RTPS header. The returned `handler` processes RTPS submessages from
;; the endpoint and maintains the endpoint table; `event!` fires
;; 'writer-discovered / 'reader-discovered / 'removed.
;; Local endpoints are announced via sedp-announce-writer! /
;; sedp-announce-reader!; SPDP events feed sedp-participant-seen!.
(define (make-sedp-domain
         #:guid-prefix prefix
         #:send! send!
         #:header! header!
         #:event! [event! (lambda (event ep) (void))])
  (check-prefix 'make-sedp-domain prefix)
  (unless (procedure? send!) (raise-argument-error 'make-sedp-domain "procedure?" send!))
  (unless (procedure? header!) (raise-argument-error 'make-sedp-domain "procedure?" header!))
  (unless (procedure? event!) (raise-argument-error 'make-sedp-domain "procedure?" event!))
  (define d
    (sedp-domain prefix header! send! (make-hash) (make-hash) (make-hash)
                 (make-hash) (make-hash) event! #f))
  ;; built-in writers: one for publications, one for subscriptions.
  ;; Built-in discovery endpoints are TRANSIENT_LOCAL by definition
  ;; (DDS-RTPS 8.5.4.1): late-joining participants must receive the
  ;; full endpoint history, so announcements survive peer acks.
  (hash-set! (sedp-domain-writers d) 'publications
             (make-reliable-writer
              #:writer-id entity-id-sedp-publications-writer
              #:writer-prefix prefix
              #:send! send!
              #:make-header header!
              #:durability 'transient-local))
  (hash-set! (sedp-domain-writers d) 'subscriptions
             (make-reliable-writer
              #:writer-id entity-id-sedp-subscriptions-writer
              #:writer-prefix prefix
              #:send! send!
              #:make-header header!
              #:durability 'transient-local))
  (values d (sedp-handler-proc d)))

(define (sedp-handler-proc d)
  (lambda (header subs sender)
    (when (sedp-domain-open? d)
      (define prefix (rtps-header-guid-prefix header))
      (for ([sub subs])
        (cond
          [(data-sub? sub)
           (route-data! d prefix sub)]
          [(heartbeat-sub? sub)
           (for-reader! d prefix (heartbeat-sub-writer-id sub)
                        (lambda (r) (reliable-reader-on-heartbeat! r sub)))]
          [(acknack-sub? sub)
           ;; addressed at one of our built-in writers
           (define wid (acknack-sub-writer-id sub))
           (define key (cond
                         [(bytes=? wid entity-id-sedp-publications-writer) 'publications]
                         [(bytes=? wid entity-id-sedp-subscriptions-writer) 'subscriptions]
                         [else #f]))
           (when key
             (define w (hash-ref (sedp-domain-writers d) key #f))
             (when w (reliable-writer-on-acknack! w sub)))]
          [(and (list? sub) (= 5 (length sub)) (eq? 'gap (car sub)))
           (for-reader! d prefix (caddr sub)
                        (lambda (r) (reliable-reader-on-gap! r (cadddr sub) (car (cddddr sub)))))])))))

(define (sedp-domain-open? d) (not (sedp-domain-closed? d)))

;; route a DATA submessage: the WRITER id tells which built-in endpoint
;; pair delivered it (readers are broadcast with readerId = unknown)
(define (route-data! d prefix sub)
  (define reader-key
    (cond
      [(bytes=? (data-sub-writer-id sub) entity-id-sedp-publications-writer)
       'publications]
      [(bytes=? (data-sub-writer-id sub) entity-id-sedp-subscriptions-writer)
       'subscriptions]
      [else #f]))
  (unless reader-key
    (dds-log-debug "sedp: DATA for an unknown built-in writer ignored"))
  (when reader-key
    (define r (get-or-create-reader! d prefix reader-key))
    (reliable-reader-on-data! r sub)))

;; get or lazily create the reliable reader following one remote
;; writer (prefix + built-in writer id); delivery parses endpoint data
(define (get-or-create-reader! d prefix reader-key)
  (define wid (if (eq? reader-key 'publications)
                  entity-id-sedp-publications-writer
                  entity-id-sedp-subscriptions-writer))
  (define rid (if (eq? reader-key 'publications)
                  entity-id-sedp-publications-reader
                  entity-id-sedp-subscriptions-reader))
  (define key (cons prefix wid))
  (hash-ref! (sedp-domain-readers d) key
             (lambda ()
               (make-reliable-reader
                #:reader-id rid
                #:writer-prefix prefix
                #:writer-id wid
                #:deliver!
                (lambda (sn payload qos)
                  (handle-endpoint-data! d prefix reader-key sn payload qos))
                #:send! (sedp-domain-send! d)
                #:make-header (sedp-domain-header! d)))))

(define (for-reader! d prefix writer-id proc)
  (define r (hash-ref (sedp-domain-readers d) (cons prefix writer-id) #f))
  (when r (proc r)))

;; decode a delivered SEDP sample into endpoint bookkeeping
(define (handle-endpoint-data! d prefix reader-key sn payload qos)
  (define kind (if (eq? reader-key 'publications) 'writer 'reader))
  (define disposed?
    (for/or ([p (in-list qos)]
             #:when (= pid-status-info (param-pid p)))
      (define-values (disposed _state) (param->status-info p))
      disposed))
  (define hash-param
    (findf (lambda (p) (= pid-key-hash (param-pid p))) qos))
  (define ep-guid
    (and hash-param (param->guid hash-param)))
  (cond
    [(and disposed? ep-guid)
     (remove-endpoint! d (guid->bytes ep-guid) 'dispose)]
    [(and payload (not (equal? payload #"")))
     (let-values ([(params scheme opts) (payload->param-list payload)])
       (define le? (odd? (bytes-ref payload 1)))
       (define ep (parse-endpoint-params params le?))
       (define full-ep
         (struct-copy discovered-endpoint ep
                      [guid (if ep-guid ep-guid (discovered-endpoint-guid ep))]
                      [kind kind]))
       (hash-set! (sedp-domain-endpoints d) (guid->bytes (discovered-endpoint-guid full-ep)) full-ep)
       ((sedp-domain-event! d)
        (if (eq? kind 'writer) 'writer-discovered 'reader-discovered)
        full-ep))]
    [else
     (dds-log-debug "sedp: sample without payload and without key hash ignored")]))

(define (remove-endpoint! d endpoint-guid-bytes reason)
  (define ep (hash-ref (sedp-domain-endpoints d) endpoint-guid-bytes #f))
  (when ep
    (hash-remove! (sedp-domain-endpoints d) endpoint-guid-bytes)
    ((sedp-domain-event! d) 'removed ep)))

;; ------------------------------------------------------------------
;; SPDP integration and local endpoint announcements

;; A participant appeared (or its locators changed): remember it as a
;; send target and push our endpoint history to it.
(define (sedp-participant-seen! d participant)
  (hash-set! (sedp-domain-participants d)
             (guid-prefix (participant-data-guid participant)) #t)
  (for ([w (in-hash-values (sedp-domain-writers d))])
    (reliable-writer-resend-history! w)))

;; A participant went away: drop its endpoints from the table.
(define (sedp-participant-gone! d participant)
  (define prefix (guid-prefix (participant-data-guid participant)))
  (hash-remove! (sedp-domain-participants d) prefix)
  (for ([guid-bytes (in-list (hash-keys (sedp-domain-endpoints d)))])
    (when (bytes=? (subbytes guid-bytes 0 12) prefix)
      (remove-endpoint! d guid-bytes 'participant-gone))))

;; Announce a local writer endpoint (records it and publishes it).
(define (sedp-announce-writer! d ep)
  (announce-endpoint! d 'publications (struct-copy discovered-endpoint ep [kind 'writer])))

;; Announce a local reader endpoint.
(define (sedp-announce-reader! d ep)
  (announce-endpoint! d 'subscriptions (struct-copy discovered-endpoint ep [kind 'reader])))

(define (announce-endpoint! d writer-key ep)
  (hash-set! (sedp-domain-locals d) (guid->bytes (discovered-endpoint-guid ep)) ep)
  (define payload
    (param-list->payload (endpoint-announcement-params ep) pl-cdr-le))
  (define w (hash-ref (sedp-domain-writers d) writer-key))
  (reliable-writer-write! w payload '() (discovered-endpoint-guid ep))
  ;; heartbeat right away: a reader that misses the DATA learns the
  ;; sequence number and nacks instead of waiting for a periodic one
  (reliable-writer-heartbeat! w))

;; Dispose a previously announced endpoint.
(define (sedp-dispose-endpoint! d ep)
  (hash-remove! (sedp-domain-locals d) (guid->bytes (discovered-endpoint-guid ep)))
  (define writer-key
    (if (eq? (discovered-endpoint-kind ep) 'writer) 'publications 'subscriptions))
  (reliable-writer-dispose! (hash-ref (sedp-domain-writers d) writer-key)
                            (discovered-endpoint-guid ep)))

;; ------------------------------------------------------------------
;; Tests: two domains over an in-memory wire

(module+ test
  (define (mk-wire)
    ;; returns (values handlers send-a send-b set-loss!) - dropping
    ;; messages is how the loss-recovery path is exercised
    (define handlers (make-hash))
    (define loss (box (lambda (which) #f)))
    (define (deliver which bs)
      (unless ((unbox loss) which)
        (define-values (header subs) (decode-rtps-message bs))
        ((hash-ref handlers which)
         header subs (sender "10.0.0.1" 7401))))
    (values handlers
            (lambda (bs) (deliver 'b bs))
            (lambda (bs) (deliver 'a bs))
            (lambda (proc) (set-box! loss proc))))

  (define prefix-a (random-guid-prefix))
  (define prefix-b (random-guid-prefix))
  (define events-a (box '()))
  (define events-b (box '()))

  (define-values (wire-handlers send-to-b send-to-a set-loss!) (mk-wire))

  (define pa
    (participant-data (make-guid prefix-a entity-id-spdp-participant)
                      '(2 . 3) (bytes 1 240) 20.0
                      default-builtin-endpoints-set
                      (udpv4-locator "10.0.0.1" 7401) #f #f #f))
  (define pb
    (participant-data (make-guid prefix-b entity-id-spdp-participant)
                      '(2 . 3) (bytes 1 240) 20.0
                      default-builtin-endpoints-set
                      (udpv4-locator "10.0.0.2" 7401) #f #f #f))

  (define-values (dom-a handler-a)
    (make-sedp-domain
     #:guid-prefix prefix-a
     #:send! send-to-b
     #:header! (lambda () (make-rtps-header prefix-a))
     #:event! (lambda (ev ep) (set-box! events-a (cons (list ev (guid->bytes (discovered-endpoint-guid ep))) (unbox events-a))))))
  (define-values (dom-b handler-b)
    (make-sedp-domain
     #:guid-prefix prefix-b
     #:send! send-to-a
     #:header! (lambda () (make-rtps-header prefix-b))
     #:event! (lambda (ev ep) (set-box! events-b (cons (list ev (guid->bytes (discovered-endpoint-guid ep))) (unbox events-b))))))

  ;; wire the handlers into the fake network
  (hash-set! wire-handlers 'a handler-a)
  (hash-set! wire-handlers 'b handler-b)

  ;; mutual participant knowledge (as SPDP would provide)
  (sedp-participant-seen! dom-a pb)
  (sedp-participant-seen! dom-b pa)

  ;; local endpoints
  (define writer-ep
    (discovered-endpoint (make-guid prefix-a (bytes 1 2 3 #x02))
                         'writer "VehicleSpeed" "SpeedType" 'reliable #f
                         (udpv4-locator "10.0.0.1" 7411)))
  (define reader-ep
    (discovered-endpoint (make-guid prefix-b (bytes 4 5 6 #x07))
                         'reader "VehicleSpeed" "SpeedType" 'best-effort #f #f))

  ;; A announces its writer; B hears it (reliable exchange)
  (sedp-announce-writer! dom-a writer-ep)
  (check-equal? (length (unbox events-b)) 1)
  (check-equal? (caar (unbox events-b)) 'writer-discovered)
  (define seen-b (hash-ref (sedp-domain-endpoints dom-b) (guid->bytes (discovered-endpoint-guid writer-ep))))
  (check-equal? (discovered-endpoint-topic seen-b) "VehicleSpeed")
  (check-equal? (discovered-endpoint-type seen-b) "SpeedType")
  (check-equal? (discovered-endpoint-reliability seen-b) 'reliable)
  (check-true (locator=? (discovered-endpoint-unicast-locator seen-b)
                         (udpv4-locator "10.0.0.1" 7411)))
  (check-equal? (discovered-endpoint-kind seen-b) 'writer)

  ;; B announces its reader; A hears it
  (sedp-announce-reader! dom-b reader-ep)
  (check-equal? (length (unbox events-a)) 1)
  (check-equal? (caar (unbox events-a)) 'reader-discovered)
  (check-equal? (discovered-endpoint-reliability
                 (hash-ref (sedp-domain-endpoints dom-a)
                           (guid->bytes (discovered-endpoint-guid reader-ep))))
                'best-effort)

  ;; loss recovery: drop A's next message; B must nack and receive the
  ;; retransmission
  (set-loss! (lambda (which) (and (eq? which 'b) (begin (set-loss! (lambda (w) #f)) #t))))
  (define writer-ep2
    (discovered-endpoint (make-guid prefix-a (bytes 7 8 9 #x02))
                         'writer "RadarTrack" "TrackType" 'reliable #f #f))
  (sedp-announce-writer! dom-a writer-ep2)
  ;; B still only knows two endpoints; the nack/retransmit resolves it
  (check-equal? (length (unbox events-b)) 2 "lost announcement recovered via nack")
  (check-equal? (discovered-endpoint-topic
                 (hash-ref (sedp-domain-endpoints dom-b)
                           (guid->bytes (discovered-endpoint-guid writer-ep2))))
                "RadarTrack")

  ;; disposal removes the endpoint on the peer
  (sedp-dispose-endpoint! dom-a writer-ep)
  (check-equal? (caar (unbox events-b)) 'removed)
  (check-false (hash-has-key? (sedp-domain-endpoints dom-b)
                              (guid->bytes (discovered-endpoint-guid writer-ep))))

  ;; endpoint announcement parsing: kind + unknown codes tolerated
  (define params (endpoint-announcement-params writer-ep))
  (define ep (parse-endpoint-params params #t))
  (check-true (guid=? (discovered-endpoint-guid ep) (discovered-endpoint-guid writer-ep)))
  (check-exn exn:fail:dds:malformed?
             (lambda () (parse-endpoint-params (list (param-string pid-topic-name "x")) #t)))

  ;; participant gone drops their endpoints
  (sedp-participant-gone! dom-a pb)
  (check-false (hash-has-key? (sedp-domain-endpoints dom-a)
                              (guid->bytes (discovered-endpoint-guid reader-ep)))))
