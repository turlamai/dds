#lang racket/base

;; Loopback acceptance test for the full stack over REAL UDP sockets:
;; two participants (A and B) on 127.0.0.1 exchange SPDP announcements,
;; discover each other's endpoints through SEDP, and move reliable
;; user data with out-of-order recovery.
;;
;; This module is excluded from `raco setup` (test-omit-paths) and is
;; meant for `raco test test/interop.rkt`. Multicast is NOT used - the
;; participants exchange announcements over unicast targets, which is
;; also how multicast-less topologies (sandboxes, VMs) are handled.
;;
;; Real-vendor interop check (manual, on a machine with a working
;; network):
;;   1. install Cyclone DDS (`brew install cyclonedds` ships
;;      `ddsperf`/`ddsls`; or build from eclipse-cyclonedds/cyclonedds)
;;   2. run `ddsls` beside a participant wired as in the README
;;      example: our SPDP announcements (239.255.0.1:7400) must make
;;      our participant appear in `ddsls`
;;   3. with Cyclone configured for unicast-only discovery
;;      (<General><Peers> pointing at our host), confirm both sides
;;      exchange SEDP and the endpoint tables fill up
;;   4. for user-data exchange, register a matching type + topic in a
;;      small Cyclone C app (topic/type names must equal the ones
;;      announced here) and confirm best-effort and reliable delivery
;;      both ways; our reliable writer answers nacks and heartbeats,
;;      our reliable reader acks

(require racket/list
         racket/set
         "../topic-type.rkt"
         "../cdr.rkt"
         "../entities.rkt"
         "../logging.rkt"
         "../message.rkt"
         "../reliable.rkt"
         "../sedp.rkt"
         "../spdp.rkt"
         "../topic.rkt"
         "../transport.rkt")

(module+ test
  (require rackunit)

  ;; wait until `proc` returns #t (bounded, to keep failures loud)
  (define (wait-until proc [timeout-secs 5.0])
    (let loop ([deadline (+ (current-inexact-milliseconds)
                            (* 1000 timeout-secs))])
      (or (proc)
          (and (< (current-inexact-milliseconds) deadline)
               (sleep 0.05)
               (loop deadline)))))

  ;; ---- participant A ------------------------------------------------
  (define events-a (box '()))
  (define received-a (box '()))
  (define ep-a
    (make-rtps-endpoint #:bind-port 0
                        #:multicast-groups '()
                        #:handler void))
  (define port-a (endpoint-udp-port ep-a))
  (define prefix-a (random-guid-prefix))

  (define spdp-w-a
    (make-spdp-writer
     #:send! (lambda (bs)
               ;; steady-state targets: B's port (added below)
               (for ([t (unbox (spdp-writer-targets spdp-w-a))])
                 (endpoint-send! ep-a (car t) (cadr t) bs))
               (endpoint-send! ep-a "127.0.0.1" port-a bs))
     #:send-target! (lambda (host port bs) (endpoint-send! ep-a host port bs))
     #:guid-prefix prefix-a
     #:metatraffic-unicast (udpv4-locator "127.0.0.1" port-a)
     #:metatraffic-multicast (udpv4-locator default-discovery-multicast-address
                                            (discovery-multicast-port 0))
     #:default-unicast (udpv4-locator "127.0.0.1" port-a)
     #:period 3600.0
     #:burst 0))

  (define-values (sedp-a sedp-handler-a)
    (make-sedp-domain
     #:guid-prefix prefix-a
     #:send! (lambda (bs)
               (for ([t (unbox (spdp-writer-targets spdp-w-a))])
                 (endpoint-send! ep-a (car t) (cadr t) bs)))
     #:header! (lambda () (make-rtps-header prefix-a))
     #:event! (lambda (ev ep) (void))))

  (define-values (spdp-r-a spdp-handler-a)
    (make-spdp-reader
     #:self-prefix prefix-a
     #:event! (lambda (ev rec reason)
                (when (eq? ev 'discovered)
                  (define met (participant-data-metatraffic-unicast rec))
                  (when (and met (locator-udpv4? met))
                    (spdp-writer-add-target! spdp-w-a (locator-ip met)
                                             (locator-port met)))
                  (sedp-participant-seen! sedp-a rec))
                (when (eq? ev 'removed)
                  (sedp-participant-gone! sedp-a rec))
                (set-box! events-a (cons ev (unbox events-a))))))

  (set-rtps-endpoint-handler!
   ep-a (lambda (header subs sndr)
          (spdp-handler-a header subs sndr)
          (sedp-handler-a header subs sndr)
          (for ([sub subs]
                #:when (data-sub? sub)
                #:when (bytes=? (data-sub-writer-id sub) (bytes 1 2 3 #x02)))
            (set-box! received-a
                      (cons (data-sub-sn sub) (unbox received-a))))))

  ;; ---- participant B ------------------------------------------------
  (define events-b (box '()))
  (define delivered-b (box '()))
  (define ep-b
    (make-rtps-endpoint #:bind-port 0
                        #:multicast-groups '()
                        #:handler void))
  (define port-b (endpoint-udp-port ep-b))
  (define prefix-b (random-guid-prefix))

  (define spdp-w-b
    (make-spdp-writer
     #:send! (lambda (bs)
               (for ([t (unbox (spdp-writer-targets spdp-w-b))])
                 (endpoint-send! ep-b (car t) (cadr t) bs))
               (endpoint-send! ep-b "127.0.0.1" port-b bs))
     #:send-target! (lambda (host port bs) (endpoint-send! ep-b host port bs))
     #:guid-prefix prefix-b
     #:metatraffic-unicast (udpv4-locator "127.0.0.1" port-b)
     #:metatraffic-multicast (udpv4-locator default-discovery-multicast-address
                                            (discovery-multicast-port 0))
     #:default-unicast (udpv4-locator "127.0.0.1" port-b)
     #:period 3600.0
     #:burst 0))

  (define-values (sedp-b sedp-handler-b)
    (make-sedp-domain
     #:guid-prefix prefix-b
     #:send! (lambda (bs)
               (for ([t (unbox (spdp-writer-targets spdp-w-b))])
                 (endpoint-send! ep-b (car t) (cadr t) bs)))
     #:header! (lambda () (make-rtps-header prefix-b))
     #:event! (lambda (ev ep) (void))))

  (define-values (spdp-r-b spdp-handler-b)
    (make-spdp-reader
     #:self-prefix prefix-b
     #:event! (lambda (ev rec reason)
                (when (eq? ev 'discovered)
                  (define met (participant-data-metatraffic-unicast rec))
                  (when (and met (locator-udpv4? met))
                    (spdp-writer-add-target! spdp-w-b (locator-ip met)
                                             (locator-port met)))
                  (sedp-participant-seen! sedp-b rec))
                (when (eq? ev 'removed)
                  (sedp-participant-gone! sedp-b rec))
                (set-box! events-b (cons ev (unbox events-b))))))

  ;; B's reliable user reader: created up front, follows A's user
  ;; writer (in a real participant the SEDP discovery event creates it)
  (define user-reader-b
    (make-reliable-reader
     #:reader-id (bytes 9 9 9 #x07)
     #:writer-prefix prefix-a
     #:writer-id (bytes 1 2 3 #x02)
     #:deliver! (lambda (sn payload qos)
                  (set-box! delivered-b (cons (cons sn payload) (unbox delivered-b))))
     #:send! (lambda (bs) (endpoint-send! ep-b "127.0.0.1" port-a bs))
     #:make-header (lambda () (make-rtps-header prefix-b))))

  (set-rtps-endpoint-handler!
   ep-b (lambda (header subs sndr)
          (spdp-handler-b header subs sndr)
          (sedp-handler-b header subs sndr)
          (for ([sub subs])
            (cond
              [(data-sub? sub)
               (when (bytes=? (data-sub-writer-id sub) (bytes 1 2 3 #x02))
                 (reliable-reader-on-data! user-reader-b sub))]
              [(heartbeat-sub? sub)
               (when (bytes=? (heartbeat-sub-writer-id sub) (bytes 1 2 3 #x02))
                 (reliable-reader-on-heartbeat! user-reader-b sub))]
              [(acknack-sub? sub)
               (void)]
              [(and (list? sub) (eq? 'gap (car sub)))
               (reliable-reader-on-gap! user-reader-b (cadddr sub) (car (cddddr sub)))]))))

  ;; ---- exchange -----------------------------------------------------
  ;; announce mutual SPDP targets: A learns B's port, B learns A's
  (spdp-writer-add-target! spdp-w-a "127.0.0.1" port-b)
  (spdp-writer-add-target! spdp-w-b "127.0.0.1" port-a)

  ;; SPDP: each side discovers the other
  (check-true (wait-until (lambda ()
                            (and (member 'discovered (unbox events-a))
                                 (member 'discovered (unbox events-b))
                                 #t)))
              "both participants discover each other over loopback UDP")

  ;; SEDP: A announces a writer endpoint, B a reader endpoint; both
  ;; tables must fill in
  (define writer-ep
    (discovered-endpoint (make-guid prefix-a (bytes 1 2 3 #x02))
                         'writer "LoopbackTopic" "LoopbackType"
                         'reliable #f
                         (udpv4-locator "127.0.0.1" port-a)))
  (define reader-ep
    (discovered-endpoint (make-guid prefix-b (bytes 9 9 9 #x07))
                         'reader "LoopbackTopic" "LoopbackType"
                         'reliable #f #f))
  (sedp-announce-writer! sedp-a writer-ep)
  (sedp-announce-reader! sedp-b reader-ep)

  (check-true (wait-until (lambda ()
                            (and (hash-ref (sedp-domain-endpoints sedp-b)
                                           (guid->bytes (discovered-endpoint-guid writer-ep)) #f)
                                 (hash-ref (sedp-domain-endpoints sedp-a)
                                           (guid->bytes (discovered-endpoint-guid reader-ep)) #f)
                                 #t)))
              "SEDP endpoint tables fill on both sides")
  (check-equal? (discovered-endpoint-topic
                 (hash-ref (sedp-domain-endpoints sedp-b)
                           (guid->bytes (discovered-endpoint-guid writer-ep))))
                "LoopbackTopic")

  ;; reliable user data with out-of-order recovery: send 1 and 3, then
  ;; a heartbeat; B nacks 2, A retransmits, all three deliver in order
  (define user-writer-a
    (make-reliable-writer
     #:writer-id (bytes 1 2 3 #x02)
     #:writer-prefix prefix-a
     #:send! (lambda (bs) (endpoint-send! ep-a "127.0.0.1" port-b bs))
     #:make-header (lambda () (make-rtps-header prefix-a))))
  ;; reliable-writer-write! returns the assigned sn - assert them so
  ;; the bare values do not print under raco test
  (check-equal? (reliable-writer-write! user-writer-a #"sample-1") 1)
  (check-equal? (reliable-writer-write! user-writer-a #"sample-2") 2)
  (check-equal? (reliable-writer-write! user-writer-a #"sample-3") 3)
  (reliable-writer-heartbeat! user-writer-a)

  (check-true (wait-until (lambda ()
                            (equal? (reverse (map car (unbox delivered-b)))
                                    '(1 2 3))))
              "reliable user data delivers in order over UDP")
  (check-equal? (map cdr (reverse (unbox delivered-b)))
                (list #"sample-1" #"sample-2" #"sample-3"))

  ;; best-effort user data: B emits a plain DATA, A's transport sees it
  (define be-msg
    (encode-rtps-message
     (make-rtps-header prefix-b)
     (list (data-sub entity-id-unknown (bytes 1 2 3 #x02) 99 '() #"be-ping" #f))))
  (endpoint-send! ep-b "127.0.0.1" port-a be-msg)
  (check-true (wait-until (lambda ()
                            (and (member 99 (unbox received-a)) #t)))
              "best-effort DATA traverses the loopback")

  ;; teardown: dispose announcements, stop machines, close sockets
  (void (sedp-dispose-endpoint! sedp-a writer-ep))
  (void (sedp-dispose-endpoint! sedp-b reader-ep))
  (void (spdp-writer-dispose! spdp-w-a))
  (void (spdp-writer-dispose! spdp-w-b))
  (spdp-reader-stop! spdp-r-a)
  (spdp-reader-stop! spdp-r-b)
  (reliable-writer-close! user-writer-a)
  (reliable-reader-close! user-reader-b)
  (endpoint-close! ep-a)
  (endpoint-close! ep-b)

  ;; ---- P3: participant/topic API over real UDP --------------------
  ;; two full participants exchange a typed user topic (reliable +
  ;; transient-local), including a late joiner receiving history
  (define speed-type
    (struct-t (struct-member 'kph u16-t)
              (struct-member 'label string-t)))
  (define p-a (make-participant #:bind-port 0))
  (define p-b (make-participant #:bind-port 0))
  (spdp-writer-add-target! (participant-spdp-w p-a) "127.0.0.1"
                           (endpoint-udp-port (participant-endpoint p-b)))
  (spdp-writer-add-target! (participant-spdp-w p-b) "127.0.0.1"
                           (endpoint-udp-port (participant-endpoint p-a)))
  (spdp-writer-announce! (participant-spdp-w p-a))
  (spdp-writer-announce! (participant-spdp-w p-b))
  (check-true (wait-until (lambda ()
                            (and (positive? (hash-count (participant-peers p-a)))
                                 (positive? (hash-count (participant-peers p-b))))))
              "participants discover each other (topic API)")

  (define t-a (participant-topic! p-a "Speed" speed-type
                                  #:durability 'transient-local
                                  #:history-depth 5))
  (define t-b (participant-topic! p-b "Speed" speed-type
                                  #:durability 'transient-local
                                  #:history-depth 5))
  (define got (box '()))
  (topic-reader! t-b (lambda (v sn)
                       (set-box! got (cons (cons sn v) (unbox got)))))
  (check-true (wait-until (lambda ()
                            (not (null? (hash-keys
                                         (writer-handle-targets
                                          (topic-writer-handle t-a)))))))
              "SEDP match converged (topic API)")
  (check-equal? (writer-write! (topic-writer! t-a) '((kph . 42) (label . "a"))) 1)
  (check-equal? (writer-write! (topic-writer! t-a) '((kph . 43) (label . "b"))) 2)
  (check-true (wait-until (lambda () (>= (length (unbox got)) 2)))
              "typed reliable topic delivers in order")
  (check-equal? (reverse (unbox got))
                (list (cons 1 '((kph . 42) (label . "a")))
                      (cons 2 '((kph . 43) (label . "b")))))

  ;; late joiner receives the transient-local history
  (define p-c (make-participant #:bind-port 0))
  (spdp-writer-add-target! (participant-spdp-w p-a) "127.0.0.1"
                           (endpoint-udp-port (participant-endpoint p-c)))
  (spdp-writer-add-target! (participant-spdp-w p-c) "127.0.0.1"
                           (endpoint-udp-port (participant-endpoint p-a)))
  (spdp-writer-announce! (participant-spdp-w p-a))
  (spdp-writer-announce! (participant-spdp-w p-c))
  (check-true (wait-until (lambda () (positive? (hash-count (participant-peers p-c)))))
              "late joiner discovers A")
  (define late-got (box '()))
  (topic-reader! (participant-topic! p-c "Speed" speed-type
                                    #:durability 'transient-local
                                    #:history-depth 5)
                 (lambda (v sn) (set-box! late-got (cons sn (unbox late-got)))))
  (check-true (wait-until (lambda () (>= (length (unbox late-got)) 2)))
              "late joiner receives the transient-local history")
  (check-equal? (reverse (unbox late-got)) '(1 2))

  (participant-close! p-a)
  (participant-close! p-b)
  (participant-close! p-c))
