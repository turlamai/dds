#lang racket/base

;; SPDP - Simple Participant Discovery Protocol (DDS-RTPS 2.3,
;; chapter 8.5 "Simple Participant Discovery Protocol" and 7.3.1).
;;
;; Every participant periodically multicasts an announcement on the
;; well-known metatraffic address/port (239.255.0.1:7400 for domain 0)
;; [8.5.1.1]. The announcement is a best-effort DATA submessage whose
;; writer is ENTITYID_SPDP_BUILTIN_PARTICIPANT, whose inlineQos carries
;; KEY_HASH + STATUS_INFO, and whose payload is a PL_CDR encapsulated
;; parameter list describing the participant: protocol version, vendor
;; id, participant guid, builtin endpoint set, locators, lease
;; duration.
;;
;; Wire rules implemented here:
;;   [8.5.1]  DATA with writerId = SPDP entity, readerId = unknown
;;   [7.3.1]  parameters per 8.5.2.2: participant guid (required);
;;            endpoints, locators, lease, version, vendor (recommended)
;;   [8.2.5]  STATUS_INFO disposal removes a participant record; a
;;            disposal carries only the inlineQos KEY_HASH, so the key
;;            comes from there when the payload is absent
;;   [8.5.2]  lease duration expiry removes a record as well; this
;;            stack also does the Cyclone-style startup burst (fast
;;            first announcements, then a steady period)

(require racket/contract
         racket/match
         "cdr.rkt"
         "entities.rkt"
         "message.rkt"
         "logging.rkt")

(provide (struct-out participant-data)
         spdp-topic-name
         spdp-announcement-params
         param-list->spdp-data
         parse-spdp-data
         (struct-out spdp-writer)
         make-spdp-writer
         spdp-writer-announce!
         spdp-writer-dispose!
         spdp-writer-stop!
         (struct-out spdp-reader)
         make-spdp-reader
         spdp-reader-lookup
         spdp-reader-stop!)

(module+ test (require rackunit racket/list "transport.rkt"))

;; Well-known topic name of the SPDP data [7.3.1.3]
(define spdp-topic-name "dcpsparticipant")

;; ------------------------------------------------------------------
;; Participant description

(struct participant-data
  (guid                ; guid?
   protocol-version    ; (cons major minor)
   vendor-id           ; 2-octet byte string
   lease-duration      ; seconds
   builtin-endpoints   ; exact integer bitmask
   metatraffic-unicast ; locator? or #f
   metatraffic-multicast
   default-unicast
   default-multicast)
  #:transparent)

(define (participant-data-guid-prefix d)
  (guid-prefix (participant-data-guid d)))


;; Build the SPDP parameter list from a participant description.
(define (spdp-announcement-params p)
  (append
   (list
    (param-protocol-version
     (car (participant-data-protocol-version p))
     (cdr (participant-data-protocol-version p)))
    (param-vendorid (participant-data-vendor-id p))
    (param-guid pid-participant-guid (participant-data-guid p))
    (param-ulonglong pid-builtin-endpoints-set
                     (participant-data-builtin-endpoints p))
    (param-double pid-participant-lease-duration
                  (participant-data-lease-duration p))
    (param-string pid-topic-name spdp-topic-name))
   (for/list ([pid+loc
               (list (cons pid-metatraffic-unicast-locator
                           (participant-data-metatraffic-unicast p))
                     (cons pid-metatraffic-multicast-locator
                           (participant-data-metatraffic-multicast p))
                     (cons pid-default-unicast-locator
                           (participant-data-default-unicast p))
                     (cons pid-default-multicast-locator
                           (participant-data-default-multicast p)))]
              #:when (locator? (cdr pid+loc)))
     (param-locator (car pid+loc) (cdr pid+loc)))))

;; SPDP DATA payload: encapsulated parameter list
(define (param-list->spdp-data params [scheme pl-cdr-le])
  (param-list->payload params scheme))

;; Parse an SPDP DATA submessage. Returns
;;   (values participant-data #f key-prefix)   full announcement
;;   (values #f disposed? key-prefix)          disposal (no payload)
;;   (values #f #f #f)                         not SPDP data
;; where key-prefix is the announced participant prefix, taken from
;; the payload guid or, for bare disposals, from the inlineQos
;; KEY_HASH. Raises exn:fail:dds:malformed for a payload without a
;; participant guid.
(define (parse-spdp-data sub)
  (unless (data-sub? sub)
    (raise-argument-error 'parse-spdp-data "data-sub?" sub))
  (if (not (bytes=? (data-sub-writer-id sub) entity-id-spdp-participant))
      (values #f #f #f)
      (let* ([qos (data-sub-qos sub)]
             [hash-param
              (findf (lambda (p) (= pid-key-hash (param-pid p))) qos)]
             [key-prefix
              (and hash-param (guid-prefix (param->guid hash-param)))]
             [disposed?
              (for/or ([p (in-list qos)]
                       #:when (= pid-status-info (param-pid p)))
                (define-values (disposed _state) (param->status-info p))
                disposed)]
             [payload (data-sub-payload sub)])
        (cond
          [(and payload (not (equal? payload #"")))
           (let-values ([(params _scheme opts) (payload->param-list payload)])
             (define data (parse-spdp-params params (odd? (bytes-ref payload 1))))
             (values data disposed?
                     (or key-prefix
                         (participant-data-guid-prefix data))))]
          [key-prefix (values #f disposed? key-prefix)]
          [else (values #f disposed? #f)]))))

;; Parse announcement parameters into a participant-data; the
;; mandatory participant guid is required [8.5.2.2]. `le?` is the
;; payload endianness.
(define (parse-spdp-params params [le? #t])
  (define (find pid)
    (findf (lambda (p) (= pid (param-pid p))) params))
  (define guid-param (find pid-participant-guid))
  (unless guid-param
    (raise (exn:fail:dds:malformed
            "dds-spdp: announcement without a participant guid"
            (current-continuation-marks))))
  (define version-param (find pid-protocol-version))
  (participant-data
   (param->guid guid-param)
   (if version-param
       (let-values ([(maj minr) (param->protocol-version version-param)])
         (cons maj minr))
       (cons rtps-version-major rtps-version-minor))
   (let ([v (find pid-vendorid)])
     (if v (param->vendorid v) (bytes 0 0)))
   (let ([v (find pid-participant-lease-duration)])
     (if v (param->double v le?) 100.0))
   (let ([v (find pid-builtin-endpoints-set)])
     (if v (param->ulonglong v le?) 0))
   (locator-param pid-metatraffic-unicast-locator find)
   (locator-param pid-metatraffic-multicast-locator find)
   (locator-param pid-default-unicast-locator find)
   (locator-param pid-default-multicast-locator find)))

(define (locator-param pid find)
  (define p (find pid))
  (and p (bytes->locator (param-value p))))

;; ------------------------------------------------------------------
;; SPDP writer (announcer)

(struct spdp-writer
  (header        ; rtps-header? announcing our vendor/prefix
   domain-id
   data           ; participant-data? what we announce
   period         ; seconds between steady-state announcements
   send!          ; (-> bytes? any/c) transport sink (multicast + targets)
   seq            ; box: announcement sequence number
   thread
   closed?)
  #:mutable)


;; Build an SPDP writer. `send!` receives fully encoded announcement
;; messages; route them to the multicast group and any unicast targets
;; (a partially applied endpoint-send-multicast! is the usual sink).
;; The writer does a Cyclone-style startup burst (`burst` fast
;; announcements), then settles to `period`. spdp-writer-stop!
;; terminates the announcements; spdp-writer-dispose! first announces
;; removal.
(define (make-spdp-writer
         #:send! send!
         #:domain-id [domain-id 0]
         #:guid-prefix [prefix #f]
         #:vendor-id [vendor-id (current-vendor-id)]
         #:lease-duration [lease-duration 30.0]
         #:builtin-endpoints [builtin-endpoints default-builtin-endpoints-set]
         #:metatraffic-unicast [metatraffic-unicast #f]
         #:metatraffic-multicast [metatraffic-multicast #f]
         #:default-unicast [default-unicast #f]
         #:default-multicast [default-multicast #f]
         #:period [period 5.0]
         #:burst [burst 3]
         #:burst-interval [burst-interval 1.0])
  (unless (procedure? send!)
    (raise-argument-error 'make-spdp-writer "procedure?" send!))
  (unless (and (real? period) (positive? period))
    (raise-argument-error 'make-spdp-writer "positive seconds" period))
  (unless (or (not prefix) (and (bytes? prefix) (= 12 (bytes-length prefix))))
    (raise-argument-error 'make-spdp-writer "12-octet guid prefix or #f" prefix))
  (define data
    (participant-data
     (make-guid (or prefix (random-guid-prefix vendor-id))
                entity-id-spdp-participant)
     (cons rtps-version-major rtps-version-minor)
     vendor-id
     lease-duration
     builtin-endpoints
     metatraffic-unicast
     metatraffic-multicast
     default-unicast
     default-multicast))
  (define w
    (spdp-writer (make-rtps-header (participant-data-guid-prefix data) vendor-id)
                 domain-id data period send! (box 0) #f #f))
  (define worker
    (thread
     (lambda ()
       ;; startup burst, then the steady period; kill-thread interrupts
       ;; the sleeps on stop
       (let burst ([left burst])
         (when (and (positive? left) (not (spdp-writer-closed? w)))
           (announce w)
           (sleep burst-interval)
           (burst (sub1 left))))
       (let steady ()
         (unless (spdp-writer-closed? w)
           (announce w)
           (sleep period)
           (steady))))))
  (set-spdp-writer-thread! w worker)
  w)

(define (announce w)
  (set-box! (spdp-writer-seq w) (add1 (unbox (spdp-writer-seq w))))
  (define msg
    (encode-rtps-message
     (spdp-writer-header w)
     (list (spdp-data-sub w (unbox (spdp-writer-seq w)) #f))))
  ((spdp-writer-send! w) msg))

;; Build the announcement (or disposal) DATA submessage
(define (spdp-data-sub w sn disposed?)
  (data-sub entity-id-unknown
            entity-id-spdp-participant
            sn
            (list (param-guid pid-key-hash
                              (participant-data-guid (spdp-writer-data w)))
                  (if disposed? status-info-disposed status-info-alive))
            (if disposed?
                #f
                (param-list->spdp-data
                 (spdp-announcement-params (spdp-writer-data w))))
            disposed?))

;; Send one announcement now.
(define (spdp-writer-announce! w) (announce w))

;; Announce disposal (removes this participant from discovery records)
;; and stop the periodic announcements.
(define (spdp-writer-dispose! w)
  (unless (spdp-writer-closed? w)
    (set-box! (spdp-writer-seq w) (add1 (unbox (spdp-writer-seq w))))
    (define msg
      (encode-rtps-message
       (spdp-writer-header w)
       (list (spdp-data-sub w (unbox (spdp-writer-seq w)) #t))))
    ((spdp-writer-send! w) msg)
    (spdp-writer-stop! w)))

(define (spdp-writer-stop! w)
  (unless (spdp-writer-closed? w)
    (set-spdp-writer-closed?! w #t)
    (when (spdp-writer-thread w)
      (kill-thread (spdp-writer-thread w)))))

;; ------------------------------------------------------------------
;; SPDP reader (detector)

;; records map: guid-prefix byte string -> (cons last-seen-ms data)
(struct spdp-reader
  (self-prefix   ; byte string or #f (loopback announcements are dropped)
   records       ; mutable hash
   event!        ; (-> (or/c 'discovered 'updated 'removed) participant-data? symbol? any/c)
   lease-thread
   stopped?)
  #:mutable)

;; Build an SPDP reader. Plug `handler` into an endpoint (it matches
;; the endpoint handler signature) to maintain the participant table;
;; `event!` receives 'discovered / 'updated / 'removed events (`removed`
;; with reason 'dispose or 'expired). Lease durations are enforced by a
;; background watchdog. spdp-reader-lookup returns a participant by
;; guid prefix; spdp-reader-stop! kills the watchdog.
(define (make-spdp-reader
         #:self-prefix [self-prefix #f]
         #:event! [event! (lambda (event record reason) (void))])
  (unless (procedure? event!)
    (raise-argument-error 'make-spdp-reader "procedure?" event!))
  (define records (make-hash))
  (define r (spdp-reader self-prefix records event! #f #f))

  (define handler
    (lambda (header subs sender)
      (when (and (spdp-reader-active? r)
                 ;; loopback of our own announcements (multicast
                 ;; loopback is on so local participants can see each
                 ;; other on one socket)
                 (or (not self-prefix)
                     (not (bytes=? (rtps-header-guid-prefix header)
                                   self-prefix)))
                 (= (rtps-header-major header) 2))
        (for ([sub subs]
              #:when (data-sub? sub)
              #:when (bytes=? (data-sub-writer-id sub)
                              entity-id-spdp-participant))
          (handle-spdp-data! r sub)))))
  (define worker
    (thread
     (lambda ()
       (let loop ()
         (sleep 1.0)
         (when (not (spdp-reader-stopped? r))
           (sweep-leases! r)
           (loop))))))
  (set-spdp-reader-lease-thread! r worker)
  (values r handler))

(define (spdp-reader-active? r) (not (spdp-reader-stopped? r)))

(define (handle-spdp-data! r sub)
  (define-values (data disposed? key-prefix) (parse-spdp-data sub))
  (unless key-prefix
    (dds-log-debug "spdp: DATA without a usable key ignored"))
  (when key-prefix
    (define records (spdp-reader-records r))
    (define now (current-inexact-milliseconds))
    (cond
      [disposed?
       (define entry (hash-ref records key-prefix #f))
       (when entry
         (hash-remove! records key-prefix)
         ((spdp-reader-event! r) 'removed (cdr entry) 'dispose))]
      [data
       (define entry (hash-ref records key-prefix #f))
       (hash-set! records key-prefix (cons now data))
       (cond
         [(not entry) ((spdp-reader-event! r) 'discovered data 'new)]
         [(not (equal? (cdr entry) data))
          ((spdp-reader-event! r) 'updated data 'changed)])])))

(define (sweep-leases! r)
  (define now (current-inexact-milliseconds))
  ;; hash-keys snapshots: removal happens while iterating
  (for ([key (in-list (hash-keys (spdp-reader-records r)))])
    (define entry (hash-ref (spdp-reader-records r) key #f))
    (when (and entry
               (< (+ (car entry)
                     (* 1000.0 (participant-data-lease-duration (cdr entry))))
                  now))
      (hash-remove! (spdp-reader-records r) key)
      ((spdp-reader-event! r) 'removed (cdr entry) 'expired))))

(define (spdp-reader-lookup r prefix)
  (define entry (hash-ref (spdp-reader-records r) prefix #f))
  (and entry (cdr entry)))

(define (spdp-reader-stop! r)
  (set-spdp-reader-stopped?! r #t)
  (when (spdp-reader-lease-thread r)
    (kill-thread (spdp-reader-lease-thread r))))

;; ------------------------------------------------------------------
;; Tests

(module+ test
  (define vendor (bytes #x01 #xF0))
  (define prefix (random-guid-prefix vendor))
  (define self-guid (make-guid prefix entity-id-spdp-participant))

  ;; a full announcement round trip through the writer's own encoder
  (define sent (box '()))
  (define w
    (make-spdp-writer
     #:send! (lambda (bs) (set-box! sent (cons bs (unbox sent))))
     #:guid-prefix prefix
     #:vendor-id vendor
     #:lease-duration 20.0
     #:metatraffic-unicast (udpv4-locator "127.0.0.1" 7401)
     #:metatraffic-multicast (udpv4-locator default-discovery-multicast-address 7400)
     #:default-unicast (udpv4-locator "127.0.0.1" 7401)
     #:default-multicast (udpv4-locator default-discovery-multicast-address 7400)
     #:period 3600.0
     #:burst 0))
  (spdp-writer-announce! w)
  (define msg (car (unbox sent)))
  (define-values (header subs) (decode-rtps-message msg))
  (check-equal? (rtps-header-guid-prefix header) prefix)
  (check-equal? (length subs) 1)
  (define spdp-sub (car subs))
  (check-equal? (data-sub-writer-id spdp-sub) entity-id-spdp-participant)
  (define-values (data disposed? key-prefix) (parse-spdp-data spdp-sub))
  (check-false disposed?)
  (check-equal? key-prefix prefix)
  (check-true (guid=? (participant-data-guid data) self-guid))
  (check-equal? (participant-data-protocol-version data) '(2 . 3))
  (check-equal? (participant-data-vendor-id data) vendor)
  (check-equal? (participant-data-lease-duration data) 20.0)
  (check-equal? (participant-data-builtin-endpoints data)
                default-builtin-endpoints-set)
  (check-true (locator=? (participant-data-metatraffic-unicast data)
                         (udpv4-locator "127.0.0.1" 7401)))
  (check-true (locator=? (participant-data-default-multicast data)
                         (udpv4-locator default-discovery-multicast-address 7400)))

  ;; the reader discovers, updates and removes the participant; it is
  ;; another local participant with its own prefix
  (define events (box '()))
  (define reader-prefix (random-guid-prefix vendor))
  (define-values (r handler)
    (make-spdp-reader
     #:self-prefix reader-prefix
     #:event! (lambda (ev rec reason)
                (set-box! events (cons (list ev (guid-prefix (participant-data-guid rec)) reason)
                                       (unbox events))))))
  ;; discover
  (handler header (list spdp-sub) (sender "192.168.1.9" 7400))
  (check-equal? (length (unbox events)) 1)
  (check-equal? (caar (unbox events)) 'discovered)
  (check-true (guid=? (participant-data-guid (spdp-reader-lookup r prefix))
                      self-guid))
  ;; loopback of the reader's own prefix is filtered out
  (define loopback-msg
    (encode-rtps-message (rtps-header 2 3 vendor reader-prefix)
                         (list spdp-sub)))
  (define-values (lb-header lb-subs) (decode-rtps-message loopback-msg))
  (handler lb-header lb-subs (sender "127.0.0.1" 7401))
  (check-equal? (length (unbox events)) 1)
  ;; identical announcement is not an event
  (handler header (list spdp-sub) (sender "192.168.1.9" 7400))
  (check-equal? (length (unbox events)) 1)
  ;; changed announcement updates (the new writer reuses the same
  ;; prefix but a different lease; announce through the shared sink)
  (spdp-writer-stop! w)
  (define w2
    (make-spdp-writer
     #:send! (lambda (bs) (set-box! sent (cons bs (unbox sent))))
     #:guid-prefix prefix
     #:vendor-id vendor
     #:lease-duration 99.0
     #:period 3600.0
     #:burst 0))
  (spdp-writer-announce! w2)
  (define-values (header2 subs2) (decode-rtps-message (car (unbox sent))))
  (handler header2 (list (car subs2)) (sender "192.168.1.9" 7400))
  (check-equal? (caar (unbox events)) 'updated)
  (check-equal? (participant-data-lease-duration (spdp-reader-lookup r prefix)) 99.0)
  (spdp-writer-stop! w2)

  ;; disposal removes the record even without a payload
  (define dispose-msg
    (encode-rtps-message
     (spdp-writer-header w2)
     (list (data-sub entity-id-unknown entity-id-spdp-participant 99
                     (list (param-guid pid-key-hash self-guid)
                           status-info-disposed)
                     #f #f))))
  (define-values (header3 subs3) (decode-rtps-message dispose-msg))
  (handler header3 (list (car subs3)) (sender "192.168.1.9" 7400))
  (check-false (spdp-reader-lookup r prefix))
  (check-equal? (caar (unbox events)) 'removed)
  (check-equal? (caddar (unbox events)) 'dispose)
  (spdp-reader-stop! r)

  ;; non-SPDP data is ignored
  (define-values (d0 disp0 key0)
    (parse-spdp-data (data-sub entity-id-unknown entity-id-unknown 1 '() #f #f)))
  (check-false d0) (check-false disp0) (check-false key0)

  ;; payload without a participant guid is malformed
  (check-exn exn:fail:dds:malformed?
             (lambda ()
               (parse-spdp-data
                (data-sub entity-id-unknown entity-id-spdp-participant 1 '()
                          (param-list->spdp-data
                           (list (param-string pid-topic-name "x")))
                          #f)))))

