#lang racket/base

;; RTPS reliable protocol subset (DDS-RTPS 2.3, chapter 8.4 "Behavior
;; Module"), transport-agnostic: writers and readers talk to the wire
;; through a caller-supplied `send!` and receive through `on-*!`
;; entry points, so the machines are testable without sockets.
;;
;; Writer side (StatefulWriter subset):
;;   [8.4.2.2.3] samples are numbered from 1 and kept in a history for
;;               retransmission; the history is pruned up to the
;;               acked-up-to point reported by an ACKNACK with final=1
;;   [8.4.2.3]  HEARTBEAT announces first/last available sequence
;;               numbers with a monotonically increasing count;
;;               final=1 tells the reader that no response is needed
;;               unless it is missing samples
;;   [8.4.2.5]  an ACKNACK with a non-empty reader SN state requests
;;               retransmission of the listed sequence numbers
;;
;; Reader side (StatefulReader subset):
;;   [8.4.2.3.3] samples are delivered in sequence-number order;
;;               out-of-order arrivals are buffered until the run is
;;               contiguous, duplicates are dropped
;;   [8.4.2.3.2] on HEARTBEAT the reader sends an ACKNACK: final=0
;;               with a bitmap when samples are missing, final=1 with
;;               an empty state when it is complete
;;   [8.4.2.3.4] a HEARTBEAT whose firstSN is ahead of the reader (or
;;               a GAP submessage) declares samples lost; the reader
;;               advances past them instead of blocking the run
;;
;; Bitmap convention (both directions, per the ecosystem: CycloneDDS
;; ddsi_reorder_nackmap / handle_gap): bit b of the bit stream
;; (counted from the MSB of the first word) marks sequence number
;; base + b (NO +1); word value bit position is 31 - (b mod 32).
;; ACKNACK: base = first non-delivered sn, set bit = missing.
;; GAP: gapStart = first missing sn, set bit = irrelevant/missing,
;; clear bit = available.

(require racket/contract
         racket/list
         racket/set
         "cdr.rkt"
         "message.rkt"
         "entities.rkt"
         "logging.rkt")

(provide (struct-out reliable-writer)
         make-reliable-writer
         reliable-writer-write!
         reliable-writer-dispose!
         reliable-writer-heartbeat!
         reliable-writer-on-acknack!
         reliable-writer-resend-history!
         reliable-writer-close!
         (struct-out reliable-reader)
         make-reliable-reader
         reliable-reader-on-data!
         reliable-reader-on-heartbeat!
         reliable-reader-on-gap!
         reliable-reader-close!
         bitmap-sns bitmap-encode)

(module+ test (require rackunit racket/list racket/set))

;; ------------------------------------------------------------------
;; Bitmap helpers (shared by writer/acknack and reader/gap decoding)

;; Decode a SequenceNumberSet into the absolute sns it marks
;; (bit b marks sn = base + b; bit b sits at word bit 31-b).
(define (bitmap-sns state)
  (define base (seqnumset-base state))
  (define nb (seqnumset-num-bits state))
  (apply
   append
   (for/list ([word (seqnumset-words state)] [wi (in-naturals)])
     (for/list ([bit (in-range 32)]
                #:when (and (< (+ (* wi 32) bit) nb)
                            (= 1 (bitwise-and (arithmetic-shift word (- bit 31)) 1))))
       (+ base (* wi 32) bit)))))

;; Encode a set of absolute sns (all >= base) relative to `base`.
;; RTPS caps numBits at 256, so sns beyond base+255 are dropped.
;; Returns (values num-bits words).
(define (bitmap-encode sns base)
  (define offsets
    (sort (for/list ([sn (in-set sns)]
                     #:when (>= sn base)
                     #:when (<= (- sn base) 255))
            (- sn base))
          <))
  (if (null? offsets)
      (values 0 '())
      (let* ([nb (add1 (foldl max -1 offsets))]
             [nwords (ceiling (/ nb 32))])
        (values nb
                (for/list ([wi (in-range nwords)])
                  (for/fold ([word 0])
                            ([g (in-list offsets)]
                             #:when (and (>= g (* wi 32)) (< g (* (add1 wi) 32))))
                    (bitwise-ior word (arithmetic-shift 1 (- 31 (- g (* wi 32)))))))))))

;; ------------------------------------------------------------------
;; Reliable writer

(struct reliable-writer
  (writer-id       ; 4-octet entity id of this endpoint
   writer-prefix   ; 12-octet participant prefix of this endpoint
   send!           ; (-> bytes? any/c) route an encoded RTPS message
   make-header     ; (-> rtps-header?) header for outbound messages
   period          ; seconds between heartbeats, #f = manual only
   durability      ; 'volatile or 'transient-local (QoS history policy)
   depth           ; keep-last depth (#f = keep-all), see prune!
   count           ; heartbeat counter
   next-sn         ; next sequence number to assign (starts at 1)
   history         ; hash: sn -> (list qos-params payload-bytes key-guid disposed?)
   last-acked      ; highest sn known acked by every reader so far
   thread
   closed?)
  #:mutable)

;; Build a reliable writer. `send!` receives fully encoded RTPS
;; messages (DATA retransmissions, HEARTBEATs); `make-header` supplies
;; the RTPS header (vendor id, guid prefix) so one writer serves one
;; endpoint. `period` schedules automatic heartbeats; pass #f to
;; drive heartbeats manually.
;;
;; History QoS (DDS-RTPS 2.3 ch. 2.2.3 / DDS-RTPS 8.4.2.2.3):
;;   `durability` 'volatile prunes acked samples immediately (the
;;   default), 'transient-local keeps samples for late-joining
;;   readers and prunes ONLY by `depth`
;;   `depth` keep-last N (#f = keep-all): samples beyond the newest N
;;   are dropped as new ones arrive, whatever the durability
(define (make-reliable-writer
         #:writer-id writer-id
         #:writer-prefix writer-prefix
         #:send! send!
         #:make-header make-header
         #:heartbeat-period [period #f]
         #:durability [durability 'volatile]
         #:history-depth [depth #f])
  (unless (procedure? send!)
    (raise-argument-error 'make-reliable-writer "procedure?" send!))
  (unless (procedure? make-header)
    (raise-argument-error 'make-reliable-writer "procedure?" make-header))
  (check-entity 'make-reliable-writer writer-id)
  (check-prefix 'make-reliable-writer writer-prefix)
  (when (and period (not (and (real? period) (positive? period))))
    (raise-argument-error 'make-reliable-writer "positive seconds or #f" period))
  (unless (memq durability '(volatile transient-local))
    (raise-argument-error 'make-reliable-writer "'volatile/'transient-local" durability))
  (when (and depth (not (exact-positive-integer? depth)))
    (raise-argument-error 'make-reliable-writer "exact positive depth or #f" depth))
  (define w
    (reliable-writer writer-id writer-prefix send! make-header period
                     durability depth
                     (box 0) (box 1) (make-hash) (box 0) #f #f))
  (when period
    (define worker
      (thread
       (lambda ()
         (let loop ()
           (sleep period)
           (unless (reliable-writer-closed? w)
             (reliable-writer-heartbeat! w)
             (loop))))))
    (set-reliable-writer-thread! w worker))
  w)

(define (writer-put! w qos payload key-guid disposed?)
  (when (reliable-writer-closed? w)
    (raise-argument-error 'writer-put! "open writer" w))
  (define sn (unbox (reliable-writer-next-sn w)))
  (set-box! (reliable-writer-next-sn w) (add1 sn))
  (hash-set! (reliable-writer-history w) sn (list qos payload key-guid disposed?))
  (prune-history! w)
  (send-data! w sn)
  sn)

;; History pruning: keep-last depth bounds the history regardless of
;; durability (oldest samples beyond the newest N go); the ack-driven
;; prune in on-acknack! only applies to volatile writers.
(define (prune-history! w)
  (define depth (reliable-writer-depth w))
  (when depth
    (define keys (sort (hash-keys (reliable-writer-history w)) <))
    (when (> (length keys) depth)
      (for ([sn keys]
            [_i (in-range (- (length keys) depth))])
        (hash-remove! (reliable-writer-history w) sn)))))

;; Publish a sample: assign the next sequence number, cache it for
;; retransmission, emit DATA (Q|D; inlineQos carries the endpoint key
;; hash). Returns the sequence number.
(define (reliable-writer-write! w payload [qos '()] [key-guid #f])
  (writer-put! w qos payload key-guid #f))

;; Emit a disposal marker (STATUS_INFO disposed in inlineQos, no
;; payload) under its own sequence number.
(define (reliable-writer-dispose! w key-guid)
  (writer-put! w (list status-info-disposed) #f key-guid #t))

(define (send-data! w sn)
  (define entry (hash-ref (reliable-writer-history w) sn))
  (define disposed? (cadddr entry))
  (define base-qos
    (if disposed?
        (cons status-info-disposed (car entry))
        (cons status-info-alive (car entry))))
  (define qos
    (if (guid? (caddr entry))
        (cons (param-guid pid-key-hash (caddr entry)) base-qos)
        base-qos))
  (define msg
    (encode-rtps-message
     ((reliable-writer-make-header w))
     (list (data-sub entity-id-unknown
                     (reliable-writer-writer-id w)
                     sn
                     qos
                     (cadr entry)
                     disposed?))))
  ((reliable-writer-send! w) msg))

;; Emit a HEARTBEAT for the current history [8.4.2.3]. An empty
;; history announces first=last=0 with the liveliness flag set (the
;; conventional empty-writer heartbeat).
(define (reliable-writer-heartbeat! w)
  (when (reliable-writer-closed? w)
    (raise-argument-error 'reliable-writer-heartbeat! "open writer" w))
  (set-box! (reliable-writer-count w) (add1 (unbox (reliable-writer-count w))))
  (define last-sn (sub1 (unbox (reliable-writer-next-sn w))))
  ;; firstSN is the lowest sample still in the history (pruning moves
  ;; it up), 0 for an empty writer; foldl keeps this exact
  (define first-sn
    (let ([keys (hash-keys (reliable-writer-history w))])
      (if (null? keys)
          0
          (foldl min (car keys) (cdr keys)))))
  (define msg
    (encode-rtps-message
     ((reliable-writer-make-header w))
     (list (heartbeat-sub entity-id-unknown
                          (reliable-writer-writer-id w)
                          first-sn
                          last-sn
                          (unbox (reliable-writer-count w))
                          #t   ; final: only respond when missing
                          (zero? last-sn)))))
  ((reliable-writer-send! w) msg))

;; Process a received ACKNACK [8.4.2.5]: bits in the reader SN state
;; request retransmission; final=1 with an empty state advances the
;; acked-up-to point and prunes the history below it.
(define (reliable-writer-on-acknack! w ack)
  (unless (acknack-sub? ack)
    (raise-argument-error 'reliable-writer-on-acknack! "acknack-sub?" ack))
  (define state (acknack-sub-reader-sn-state ack))
  (define requested (bitmap-sns state))
  (for ([sn (remove-duplicates requested)])
    (when (hash-has-key? (reliable-writer-history w) sn)
      (send-data! w sn)))
  ;; final=1 with an empty state: the reader holds everything below
  ;; the bitmap base. Volatile writers prune acked samples right away;
  ;; transient-local writers keep them for late joiners and rely on
  ;; keep-last depth alone. NOTE: no heartbeat here - the periodic
  ;; thread covers the cadence, and heartbeating on acks livelocks
  ;; (heartbeat -> final ack -> heartbeat -> ...) once the reader is
  ;; complete.
  (when (and (acknack-sub-final? ack) (null? requested))
    (define base (seqnumset-base state))
    (when (> (sub1 base) (unbox (reliable-writer-last-acked w)))
      (set-box! (reliable-writer-last-acked w) (sub1 base))
      (when (eq? (reliable-writer-durability w) 'volatile)
        (for ([sn (in-list (hash-keys (reliable-writer-history w)))])
          (when (< sn base)
            (hash-remove! (reliable-writer-history w) sn)))))))

;; Re-send every sample in the history plus a heartbeat - used when a
;; new remote reader appears (a late-joining participant must receive
;; the whole endpoint history, not just future samples).
(define (reliable-writer-resend-history! w)
  (when (reliable-writer-closed? w)
    (raise-argument-error 'reliable-writer-resend-history! "open writer" w))
  (for ([sn (sort (hash-keys (reliable-writer-history w)) <)])
    (send-data! w sn))
  (when (positive? (hash-count (reliable-writer-history w)))
    (reliable-writer-heartbeat! w)))

(define (reliable-writer-close! w)
  (unless (reliable-writer-closed? w)
    (set-reliable-writer-closed?! w #t)
    (when (reliable-writer-thread w)
      (kill-thread (reliable-writer-thread w)))))

;; ------------------------------------------------------------------
;; Reliable reader

(struct reliable-reader
  (reader-id       ; 4-octet entity id of this endpoint
   writer-prefix   ; 12-octet prefix of the remote writer we follow
   writer-id       ; 4-octet entity id of the remote writer
   deliver!        ; (-> sn payload-bytes qos-params any/c) in-order sample
   send!           ; (-> bytes? any/c) route ACKNACKs
   make-header     ; (-> rtps-header?) header for outbound acknacks
   delivered       ; highest delivered sn
   buffered        ; hash: sn -> data-sub waiting for its turn
   missing         ; set of sns declared present but not yet received
   skipped         ; set of sns declared never-coming (gap/base jumps)
   ack-count       ; acknack counter
   closed?)
  #:mutable)

;; Build a reliable reader following one remote writer
;; (`writer-prefix` + `writer-id`; the prefix arrives in the RTPS
;; header, routing happens in the caller). Samples arrive via
;; reliable-reader-on-data!, writer progress via -on-heartbeat!,
;; explicit loss notices via -on-gap!; contiguous samples are handed
;; to `deliver!` in sequence-number order.
(define (make-reliable-reader
         #:reader-id reader-id
         #:writer-prefix writer-prefix
         #:writer-id writer-id
         #:deliver! deliver!
         #:send! send!
         #:make-header make-header)
  (check-entity 'make-reliable-reader reader-id)
  (check-prefix 'make-reliable-reader writer-prefix)
  (check-entity 'make-reliable-reader writer-id)
  (unless (procedure? deliver!)
    (raise-argument-error 'make-reliable-reader "procedure?" deliver!))
  (unless (procedure? send!)
    (raise-argument-error 'make-reliable-reader "procedure?" send!))
  (unless (procedure? make-header)
    (raise-argument-error 'make-reliable-reader "procedure?" make-header))
  (reliable-reader reader-id writer-prefix writer-id deliver! send! make-header
                   (box 0) (make-hash) (set) (set) (box 0) #f))

;; Deliver the contiguous run from delivered+1 while it holds; sns in
;; `skipped` (declared never-coming) advance the floor without
;; delivering.
(define (advance-delivery! r)
  (define delivered (reliable-reader-delivered r))
  (define buffered (reliable-reader-buffered r))
  (let loop ()
    (define next (add1 (unbox delivered)))
    (cond
      [(set-member? (reliable-reader-skipped r) next)
       (set-reliable-reader-skipped!
        r (set-remove (reliable-reader-skipped r) next))
       (set-box! delivered next)
       (loop)]
      [(hash-ref buffered next #f)
       => (lambda (sub)
            (hash-remove! buffered next)
            (set-reliable-reader-missing!
             r (set-remove (reliable-reader-missing r) next))
            (set-box! delivered next)
            ((reliable-reader-deliver! r)
             next (data-sub-payload sub) (data-sub-qos sub))
            (loop))]
      [else (void)])))

;; Drop every trace of sns <= bound from the floor state.
(define (drop-below! r bound)
  (set-box! (reliable-reader-delivered r)
            (max (unbox (reliable-reader-delivered r)) bound))
  (for ([sn (in-list (hash-keys (reliable-reader-buffered r)))])
    (when (<= sn bound)
      (hash-remove! (reliable-reader-buffered r) sn)))
  (set-reliable-reader-missing!
   r (for/set ([sn (in-set (reliable-reader-missing r))]
               #:when (> sn bound))
       sn))
  (set-reliable-reader-skipped!
   r (for/set ([sn (in-set (reliable-reader-skipped r))]
               #:when (> sn bound))
       sn)))

;; Handle a DATA submessage from the followed writer [8.4.2.3.3].
(define (reliable-reader-on-data! r sub)
  (unless (data-sub? sub)
    (raise-argument-error 'reliable-reader-on-data! "data-sub?" sub))
  (unless (bytes=? (data-sub-writer-id sub) (reliable-reader-writer-id r))
    (raise-argument-error 'reliable-reader-on-data! "DATA from the followed writer" sub))
  (define sn (data-sub-sn sub))
  (cond
    [(<= sn (unbox (reliable-reader-delivered r))) (void)] ; below the run
    [(hash-has-key? (reliable-reader-buffered r) sn) (void)] ; duplicate
    [else
     (hash-set! (reliable-reader-buffered r) sn sub)
     (advance-delivery! r)]))

;; Handle a GAP submessage [8.4.2.3.4]: gapStart and the SET bits of
;; the gap list (sn = base + b) are declared irrelevant - they will
;; never arrive. They move to `skipped` so the run can advance past
;; them. CLEAR bits mean available and are left untouched.
(define (reliable-reader-on-gap! r gap-start gap-list)
  (define gone
    (cons gap-start (bitmap-sns gap-list)))
  (define delivered (unbox (reliable-reader-delivered r)))
  (for ([sn (in-list gone)]
        #:when (> sn delivered))
    (set-reliable-reader-missing!
     r (set-remove (reliable-reader-missing r) sn))
    (set-reliable-reader-skipped! r (set-add (reliable-reader-skipped r) sn)))
  (advance-delivery! r))

;; Handle a HEARTBEAT [8.4.2.3.2]: reconcile what the writer has,
;; declare samples below firstSN lost, request the missing ones.
(define (reliable-reader-on-heartbeat! r hb)
  (unless (heartbeat-sub? hb)
    (raise-argument-error 'reliable-reader-on-heartbeat! "heartbeat-sub?" hb))
  (define first (heartbeat-sub-first-sn hb))
  (define last (heartbeat-sub-last-sn hb))
  (when (zero? last)
    ;; empty writer announcement: nothing to fetch
    (send-acknack! r #t '())
    (void))
  (when (positive? last)
    ;; samples below firstSN can no longer be obtained [8.2.2.3]
    (when (> (sub1 first) (unbox (reliable-reader-delivered r)))
      (drop-below! r (sub1 first)))
    ;; reconcile the missing set against [delivered+1, last]; skipped
    ;; sns are gone, not missing
    (define have (list->set (hash-keys (reliable-reader-buffered r))))
    (define floor (unbox (reliable-reader-delivered r)))
    (set-reliable-reader-missing!
     r
     (for/set ([sn (in-range (add1 floor) (add1 last))]
               #:when (not (set-member? have sn))
               #:when (not (set-member? (reliable-reader-skipped r) sn)))
       sn))
    (advance-delivery! r)
    (define still-missing (reliable-reader-missing r))
    (if (set-empty? still-missing)
        (send-acknack! r #t '())
        (send-acknack! r #f still-missing))))

;; Send an ACKNACK: final=#t with an empty state when complete,
;; final=#f with a bitmap over the missing set otherwise [8.3.5.4.6].
;; The bitmap base is the first non-delivered sn (ecosystem
;; convention: bit i marks base + i).
(define (send-acknack! r final missing-set)
  (set-box! (reliable-reader-ack-count r)
            (add1 (unbox (reliable-reader-ack-count r))))
  (define base (add1 (unbox (reliable-reader-delivered r))))
  (define-values (nb words)
    (if (set-empty? missing-set)
        (values 0 '())
        (bitmap-encode missing-set base)))
  (define msg
    (encode-rtps-message
     ((reliable-reader-make-header r))
     (list (acknack-sub (reliable-reader-reader-id r)
                        (reliable-reader-writer-id r)
                        (seqnumset base nb words)
                        (unbox (reliable-reader-ack-count r))
                        final))))
  ((reliable-reader-send! r) msg))

(define (reliable-reader-close! r)
  (set-reliable-reader-closed?! r #t))

;; ------------------------------------------------------------------
;; Tests

(module+ test
  ;; bitmap round trip: bit b marks sn = base + b
  (define st (seqnumset 10 32 (list #x80000001)))
  (check-equal? (bitmap-sns st) '(10 41) "bit b marks sn = base + b")
  (define-values (nb words) (bitmap-encode (set 10 41 42) 10))
  (check-equal? nb 33)
  (check-equal? (bitmap-sns (seqnumset 10 nb words)) '(10 41 42))
  (define-values (nb0 words0) (bitmap-encode (set 5) 10))
  (check-equal? (list nb0 words0) (list 0 '()) "sns below base encode empty")
  (define sns-big (list->set (range 10 400)))
  (define-values (nb-big _words-big) (bitmap-encode sns-big 10))
  (check-equal? nb-big 256 "numBits clamps to the RTPS maximum")

  ;; writer: numbering, heartbeat, nack retransmit, history pruning
  (define sent (box '()))
  (define w (make-reliable-writer
             #:writer-id entity-id-sedp-publications-writer
             #:writer-prefix (make-bytes 12 #x33)
             #:send! (lambda (bs) (set-box! sent (cons bs (unbox sent))))
             #:make-header (lambda () (make-rtps-header (make-bytes 12 #x33)))
             #:heartbeat-period #f))
  (check-equal? (reliable-writer-write! w #"one") 1)
  (check-equal? (reliable-writer-write! w #"two") 2)
  (check-equal? (reliable-writer-write! w #"three") 3)
  (reliable-writer-heartbeat! w)
  (define wire (reverse (unbox sent)))
  (define-values (h1 subs1) (decode-rtps-message (list-ref wire 0)))
  (check-equal? (data-sub-sn (car subs1)) 1)
  (define-values (_h2 subs2) (decode-rtps-message (list-ref wire 3)))
  (define hb (car subs2))
  (check-true (heartbeat-sub? hb))
  (check-equal? (heartbeat-sub-first-sn hb) 1)
  (check-equal? (heartbeat-sub-last-sn hb) 3)

  ;; a nack for sn 2 (base 2, bit 0) retransmits exactly that sample
  (reliable-writer-on-acknack!
   w (acknack-sub entity-id-sedp-publications-reader
                  entity-id-sedp-publications-writer
                  (seqnumset 2 1 (list #x80000000))
                  1 #f))
  (define-values (_h3 subs3) (decode-rtps-message (car (unbox sent))))
  (check-equal? (data-sub-sn (car subs3)) 2 "nacked sn is retransmitted")

  ;; final acknack prunes the history below the base
  (reliable-writer-on-acknack!
   w (acknack-sub entity-id-sedp-publications-reader
                  entity-id-sedp-publications-writer
                  (seqnumset 4 0 '()) 2 #t))
  (check-false (hash-has-key? (reliable-writer-history w) 1))
  (check-false (hash-has-key? (reliable-writer-history w) 3))
  ;; the pruned writer keeps serving new samples from sn 4
  (check-equal? (reliable-writer-write! w #"four") 4)
  (reliable-writer-heartbeat! w)
  (define-values (_h4 subs4) (decode-rtps-message (car (unbox sent))))
  (check-equal? (heartbeat-sub-first-sn (car subs4)) 4
                "firstSN follows history pruning")
  (reliable-writer-close! w)

  ;; reader: reordering, duplicates, nack, gap skip, lost-below-base
  (define delivered (box '()))
  (define acks (box '()))
  (define prefix (make-bytes 12 #x44))
  (define r (make-reliable-reader
             #:reader-id entity-id-sedp-publications-reader
             #:writer-prefix prefix
             #:writer-id entity-id-sedp-publications-writer
             #:deliver! (lambda (sn payload qos)
                          (set-box! delivered (cons (list sn payload) (unbox delivered))))
             #:send! (lambda (bs) (set-box! acks (cons bs (unbox acks))))
             #:make-header (lambda () (make-rtps-header (make-bytes 12 #x55)))))
  (define (mk-data sn) (data-sub entity-id-unknown entity-id-sedp-publications-writer sn '() #f #f))
  (reliable-reader-on-data! r (mk-data 1))
  (reliable-reader-on-data! r (mk-data 3))
  (check-equal? (map car (reverse (unbox delivered))) '(1)
                "in-order prefix delivers immediately")
  (reliable-reader-on-heartbeat! r (heartbeat-sub entity-id-unknown
                                                  entity-id-sedp-publications-writer 1 3 1 #f #f))
  (define-values (_ha acka) (decode-rtps-message (car (unbox acks))))
  (define ack (car acka))
  (check-false (acknack-sub-final? ack) "missing samples request a retransmit")
  ;; base = first non-delivered (2); sn 3 is already buffered, so only
  ;; sn 2 (bit 0) is requested
  (check-equal? (seqnumset-base (acknack-sub-reader-sn-state ack)) 2)
  (check-equal? (bitmap-sns (acknack-sub-reader-sn-state ack)) '(2))
  ;; the nacked sample arrives: the run completes in order
  (reliable-reader-on-data! r (mk-data 2))
  (check-equal? (map car (reverse (unbox delivered))) '(1 2 3))

  ;; duplicate is ignored
  (reliable-reader-on-data! r (mk-data 2))
  (check-equal? (length (unbox delivered)) 3)

  ;; complete state acks final with an empty bitmap
  (reliable-reader-on-heartbeat! r (heartbeat-sub entity-id-unknown
                                                  entity-id-sedp-publications-writer 1 3 2 #f #f))
  (define-values (_hb ackb) (decode-rtps-message (car (unbox acks))))
  (check-true (acknack-sub-final? (car ackb)))
  (check-equal? (seqnumset-num-bits (acknack-sub-reader-sn-state (car ackb))) 0)

  ;; heartbeat base ahead of us: samples 4..5 declared lost, run skips
  (reliable-reader-on-heartbeat! r (heartbeat-sub entity-id-unknown
                                                  entity-id-sedp-publications-writer 6 6 3 #f #f))
  (reliable-reader-on-data! r (mk-data 6))
  (check-equal? (map car (reverse (unbox delivered))) '(1 2 3 6)
                "samples below the heartbeat base are skipped")

  ;; an explicit GAP declares 7..8 missing-forever and unblocks the run
  (reliable-reader-on-heartbeat! r (heartbeat-sub entity-id-unknown
                                                  entity-id-sedp-publications-writer 7 9 4 #f #f))
  (define-values (_hc ackc) (decode-rtps-message (car (unbox acks))))
  (check-equal? (bitmap-sns (acknack-sub-reader-sn-state (car ackc))) '(7 8 9))
  ;; gapStart 7; gap list base 8 with bit 0 set (sn 8)
  (reliable-reader-on-gap! r 7 (seqnumset 8 1 (list #x80000000)))
  (reliable-reader-on-data! r (mk-data 9))
  (check-equal? (map car (reverse (unbox delivered))) '(1 2 3 6 9)
                "gap-declared sns never block the run")
  (reliable-reader-close! r)

  ;; QoS: keep-last depth bounds the history whatever the ack state
  (define wl (make-reliable-writer
              #:writer-id entity-id-sedp-publications-writer
              #:writer-prefix (make-bytes 12 #x33)
              #:send! void
              #:make-header (lambda () (make-rtps-header (make-bytes 12 #x33)))
              #:durability 'transient-local
              #:history-depth 2))
  (check-equal? (reliable-writer-write! wl #"a") 1)
  (check-equal? (reliable-writer-write! wl #"b") 2)
  (check-equal? (reliable-writer-write! wl #"c") 3)
  (check-equal? (sort (hash-keys (reliable-writer-history wl)) <) '(2 3)
                "keep-last 2 retains only the newest samples")
  (reliable-writer-close! wl)

  ;; QoS: volatile prunes acked samples, transient-local keeps them
  (define (mk-w dur)
    (make-reliable-writer
     #:writer-id entity-id-sedp-publications-writer
     #:writer-prefix (make-bytes 12 #x33)
     #:send! void
     #:make-header (lambda () (make-rtps-header (make-bytes 12 #x33)))
     #:durability dur))
  (define wv (mk-w 'volatile))
  (check-equal? (reliable-writer-write! wv #"one") 1)
  (check-equal? (reliable-writer-write! wv #"two") 2)
  (reliable-writer-on-acknack!
   wv (acknack-sub entity-id-sedp-publications-reader
                   entity-id-sedp-publications-writer
                   (seqnumset 3 0 '()) 1 #t))
  (check-equal? (hash-count (reliable-writer-history wv)) 0
                "volatile writer drops fully acked samples")
  (reliable-writer-close! wv)
  (define wt (mk-w 'transient-local))
  (check-equal? (reliable-writer-write! wt #"one") 1)
  (check-equal? (reliable-writer-write! wt #"two") 2)
  (reliable-writer-on-acknack!
   wt (acknack-sub entity-id-sedp-publications-reader
                   entity-id-sedp-publications-writer
                   (seqnumset 3 0 '()) 1 #t))
  (check-equal? (sort (hash-keys (reliable-writer-history wt)) <) '(1 2)
                "transient-local writer retains acked samples for late joiners")
  (reliable-writer-close! wt))
