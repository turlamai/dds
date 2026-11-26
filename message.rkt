#lang racket/base

;; RTPS wire format (DDS-RTPS 2.3, chapter 8.3 "Message interpretation"
;; and chapter 9 "Data formats").
;;
;; Layers:
;;   header     "RTPS" magic + protocol version + vendor id + guid prefix,
;;              20 octets, always big endian [9.4.4.4]
;;   submessage kind octet + flags octet + octetsToNextHeader (u16,
;;              byte order given by flag bit 0); 0 marks the LAST
;;              submessage, whose content then extends to the end of
;;              the datagram [9.4.5.1]
;;   content    per kind, endianness of flag bit 0
;;
;; Submessages implemented here:
;;   DATA       0x15 - user/builtin data (SPDP, SEDP, topics)
;;   HEARTBEAT  0x1c - reliable writer progress [8.3.5.4.8]
;;   ACKNACK    0x06 - reliable reader feedback [8.3.5.4.6]
;;   GAP        0x18 - writer-side gap report [8.3.5.4.5]
;;   INFO_TS    0x09 - source timestamp [8.3.5.4.11]
;;   INFO_DST   0x0e - destination participant [8.3.5.4.12]
;; Unknown kinds are skipped using octetsToNextHeader.
;;
;; DATA layout [9.4.5.3.2]:
;;   extraFlags (u16, 0) + octetsToInlineQos (u16, offset from the
;;   start of readerId) + readerId (4) + writerId (4) + writerSN (8) +
;;   [inlineQos parameters] + [payload]
;;
;; Parameters [9.4.2.9]:
;;   PID (u16) + length (u16) + value + zero padding to a multiple of
;;   4; the length counts the value octets, not the padding. Lists end
;;   at PID_SENTINEL. Encapsulated lists (discovery payloads) carry the
;;   PL_CDR encapsulation header.

(require racket/contract
         racket/format
         racket/match
         "cdr.rkt"
         "entities.rkt"
         "private/bytes.rkt"
         "logging.rkt")

(provide (struct-out rtps-header)
         rtps-version-major rtps-version-minor
         default-vendor-id
         current-vendor-id
         make-rtps-header

         (struct-out param)
         pid-pad pid-sentinel pid-topic-name pid-type-name
         pid-protocol-version pid-vendorid
         pid-reliability pid-liveliness pid-durability
         pid-ownership pid-presentation pid-user-data pid-topic-data
         pid-participant-guid
         pid-unicast-locator pid-multicast-locator
         pid-metatraffic-unicast-locator pid-metatraffic-multicast-locator
         pid-default-unicast-locator pid-default-multicast-locator
         pid-expects-inline-qos
         pid-builtin-endpoints-set pid-participant-lease-duration
         pid-endpoint-guid
         pid-key-hash pid-status-info
         param-locator param-guid param-string param-ulong param-ulonglong
         param-double param-protocol-version param-vendorid param-status-info
         param-reliability param-durability param-endpoint-guid
         param->locator param->guid param->string param->ulong
         param->ulonglong param->double param->protocol-version param->vendorid
         param->status-info param->reliability param->durability
         reliability-best-effort reliability-reliable
         durability-volatile durability-transient-local
         durability-transient durability-persistent
         status-info-alive status-info-disposed
         encode-params decode-params param-list->payload payload->param-list)

(provide (struct-out info-ts)
         (struct-out info-dst)
         (struct-out data-sub)
         (struct-out heartbeat-sub)
         (struct-out acknack-sub)
         (struct-out seqnumset)
         sn->cursor cursor-sn
         seqnumset->cursor cursor-seqnumset

         encode-rtps-message
         decode-rtps-message)

(module+ test (require rackunit))

;; ------------------------------------------------------------------
;; Header

(struct rtps-header (major minor vendor-id guid-prefix) #:transparent)

(define rtps-version-major 2)
(define rtps-version-minor 3)

;; Vendor id 0x01F0: the 0x01xx block is OMG-assigned; 0xF0 sits past
;; the assigned range. Make it a parameter so tests and bridges can
;; pose as a specific vendor if ever needed.
(define default-vendor-id (bytes #x01 #xF0))
(define current-vendor-id (make-parameter default-vendor-id))

(define (make-rtps-header guid-prefix [vendor-id (current-vendor-id)])
  (check-prefix 'make-rtps-header guid-prefix)
  (unless (and (bytes? vendor-id) (= 2 (bytes-length vendor-id)))
    (raise-argument-error 'make-rtps-header "2-octet vendor id" vendor-id))
  (rtps-header rtps-version-major rtps-version-minor vendor-id guid-prefix))

;; ------------------------------------------------------------------
;; Parameters

(struct param (pid value) #:transparent)

;; ParameterId values per DDS-RTPS 2.3 table 9.32, cross-checked
;; against the CycloneDDS enum (src/core/ddsi/src/ddsi__protocol.h)
;; and the Wireshark RTPS dissector; the locator PIDs sit at
;; 0x2f-0x33/0x48 (NOT 0x51-0x54, which are participant/group
;; identity PIDs) and the lease duration is 0x2.
(define pid-pad 0)
(define pid-sentinel 1)
(define pid-participant-lease-duration #x0002)
(define pid-topic-name #x0005)
(define pid-type-name #x0007)
(define pid-protocol-version #x0015)
(define pid-vendorid #x0016)
(define pid-reliability #x001a)
(define pid-liveliness #x001b)
(define pid-durability #x001d)
(define pid-ownership #x001f)
(define pid-presentation #x0021)
(define pid-user-data #x002c)
(define pid-topic-data #x002e)
(define pid-unicast-locator #x002f)
(define pid-multicast-locator #x0030)
(define pid-default-unicast-locator #x0031)
(define pid-metatraffic-unicast-locator #x0032)
(define pid-metatraffic-multicast-locator #x0033)
(define pid-expects-inline-qos #x0043)
(define pid-default-multicast-locator #x0048)
(define pid-participant-guid #x0050)
(define pid-builtin-endpoints-set #x0058)
(define pid-endpoint-guid #x005a)
(define pid-key-hash #x0070)
(define pid-status-info #x0071)

;; value builders

(define (param-locator pid loc) (param pid (locator->bytes loc)))
(define (param-guid pid g) (param pid (guid->bytes g)))
(define (param-string pid s)
  (param pid (if (string? s) (string->bytes/utf-8 s) s)))
(define (param-ulong pid v)
  (check-parameter-width 'param-ulong pid v 4)
  (param pid (integer->bytes v 4 #t)))
(define (param-ulonglong pid v)
  (check-parameter-width 'param-ulonglong pid v 8)
  (param pid (integer->bytes v 8 #t)))
(define (param-double pid v)
  (param pid (real->floating-point-bytes (exact->inexact v) 8 #f)))
(define (param-protocol-version [major rtps-version-major] [minor rtps-version-minor])
  (param pid-protocol-version (bytes major minor 0 0)))
(define (param-vendorid [vendor-id (current-vendor-id)])
  (unless (and (bytes? vendor-id) (= 2 (bytes-length vendor-id)))
    (raise-argument-error 'param-vendorid "2-octet vendor id" vendor-id))
  (param pid-vendorid (bytes-append vendor-id (bytes 0 0))))
(define (param-status-info [disposed? #f])
  ;; StatusInfo_t: 4 octets of flags (bit 0 = disposed) + 4 octets of
  ;; instance state (0 = alive, 1 = disposed) [7.2.5]
  (param pid-status-info
         (if disposed?
             (bytes 1 0 0 0  1 0 0 0)
             (bytes 0 0 0 0  0 0 0 0))))

(define status-info-alive (param-status-info #f))
(define status-info-disposed (param-status-info #t))

;; ------------------------------------------------------------------
;; QoS parameter values (the small subset SEDP announces)

(define reliability-best-effort 1)
(define reliability-reliable 2)
(define durability-volatile 1)
(define durability-transient-local 2)
(define durability-transient 3)
(define durability-persistent 4)

(define (reliability-kind-code kind)
  (case kind
    [(best-effort) reliability-best-effort]
    [(reliable) reliability-reliable]
    [else (raise-argument-error 'param-reliability "'best-effort/'reliable" kind)]))

;; Duration_t on the wire: seconds (i32) + nanoseconds (u32)
(define (duration->bytes secs)
  (define s (exact->inexact secs))
  (define whole (inexact->exact (floor s)))
  (define nanos (inexact->exact (floor (* 1000000000 (- s whole)))))
  (bytes-append (integer->bytes whole 4 #t)
                (integer->bytes nanos 4 #t)))

;; ReliabilityQosPolicy: kind (u32) + max blocking time (Duration_t)
(define (param-reliability [kind 'reliable] [max-blocking-secs 0.1])
  (param pid-reliability
         (bytes-append (integer->bytes (reliability-kind-code kind) 4 #t)
                       (duration->bytes max-blocking-secs))))

;; DurabilityQosPolicy: kind (u32)
(define (param-durability [kind 'volatile])
  (define code
    (case kind
      [(volatile) durability-volatile]
      [(transient-local) durability-transient-local]
      [(transient) durability-transient]
      [(persistent) durability-persistent]
      [else (raise-argument-error 'param-durability
                                  "'volatile/'transient-local/'transient/'persistent"
                                  kind)]))
  (param pid-durability (integer->bytes code 4 #t)))

;; endpoint announcement helper: just param-guid with PID_ENDPOINT_GUID
(define (param-endpoint-guid g) (param-guid pid-endpoint-guid g))

(define (check-parameter-width who pid v width)
  (define lim (arithmetic-shift 1 (* 8 width)))
  (unless (and (exact-nonnegative-integer? v) (< v lim))
    (raise-argument-error who (format "uint~a" (* 8 width)) v)))

(define (integer->bytes v n le?)
  (define bs (make-bytes n))
  (for ([i (in-range n)])
    (define shift (if le? (* 8 i) (* 8 (sub1 (- n i)))))
    (bytes-set! bs i (bitwise-and (arithmetic-shift v shift) #xFF)))
  bs)

;; value readers (endian-sensitive)

(define (param->locator p) (bytes->locator (param-value p)))
(define (param->guid p) (bytes->guid (param-value p)))
(define (param->string p) (bytes->string/utf-8 (param-value p)))
;; multi-octet parameter values follow the PAYLOAD endianness; these
;; readers take it explicitly, defaulting to little endian (the
;; ecosystem norm for discovery payloads)
(define (param->ulong p [le? #t])
  (if le? (bytes->u32le (param-value p)) (bytes->u32 (param-value p))))
(define (param->ulonglong p [le? #t])
  (if le?
      (bytes->u64le (param-value p))
      (let ([bs (param-value p)])
        (for/fold ([v 0]) ([i (in-range 8)])
          (+ (arithmetic-shift v 8) (bytes-ref bs i))))))
(define (param->double p [le? #t])
  (floating-point-bytes->real (param-value p) (not le?)))
(define (param->protocol-version p)
  (values (bytes-ref (param-value p) 0) (bytes-ref (param-value p) 1)))
(define (param->vendorid p) (subbytes (param-value p) 0 2))
(define (param->status-info p)
  ;; returns (values disposed? instance-state)
  (values (= 1 (bytes-ref (param-value p) 0))
          (bytes-ref (param-value p) 4)))

(define (param->reliability p [le? #t])
  ;; returns (values kind max-blocking-secs); kind is the raw policy
  ;; code (1 = best effort, 2 = reliable)
  (define bs (param-value p))
  (define (u32-at off)
    (if le?
        (+ (bytes-ref bs off)
           (arithmetic-shift (bytes-ref bs (+ off 1)) 8)
           (arithmetic-shift (bytes-ref bs (+ off 2)) 16)
           (arithmetic-shift (bytes-ref bs (+ off 3)) 24))
        (bytes->u32 bs off)))
  (values (u32-at 0)
          (+ (u32-at 4) (/ (u32-at 8) 1e9))))

(define (param->durability p [le? #t])
  ;; returns the raw policy code (1 volatile, 2 transient-local, ...)
  (param->ulong p le?))

(define (bytes->u32le bs [off 0])
  (+ (bytes-ref bs off)
     (arithmetic-shift (bytes-ref bs (+ off 1)) 8)
     (arithmetic-shift (bytes-ref bs (+ off 2)) 16)
     (arithmetic-shift (bytes-ref bs (+ off 3)) 24)))

(define (bytes->u64le bs [off 0])
  (for/fold ([v 0]) ([i (in-range 8)])
    (+ v (arithmetic-shift (bytes-ref bs (+ off i)) (* 8 i)))))

;; encode a list of parameters into raw parameter entries (no sentinel,
;; no encapsulation) - used for inlineQos
(define (encode-params c params)
  (for ([p params])
    (unless (param? p) (raise-argument-error 'encode-params "param?" p))
    (define v (param-value p))
    (cur-u16 c (param-pid p))
    (cur-u16 c (bytes-length v))
    (cur-raw c v)
    (define pad (modulo (- 4 (modulo (bytes-length v) 4)) 4))
    (when (positive? pad) (cur-fill c pad))))

;; decode parameter entries until PID_SENTINEL or end of reader
(define (decode-params r)
  (let loop ([acc '()])
    (cond
      [(rd-eof? r) (reverse acc)]
      [(< (rd-remaining r) 4)
       ;; truncated trailing entry: keep what parsed, warn
       (dds-log-warning "message: truncated parameter entry (~a octets left)"
                        (rd-remaining r))
       (reverse acc)]
      [else
       (define pid (rd-u16 r))
       (define len (rd-u16 r))
       (cond
         [(= pid pid-sentinel) (reverse acc)]
         [(> len (rd-remaining r))
          (dds-log-warning "message: parameter 0x~x length ~a overruns the list"
                           pid len)
          (reverse acc)]
         [else
          (define value (rd-raw r len))
          (rd-skip r (modulo (- 4 (modulo len 4)) 4))
          (loop (cons (param pid value) acc))])])))

;; Encapsulated parameter list (discovery payloads): PL_CDR_LE/BE
(define (param-list->payload params [scheme pl-cdr-le])
  (cdr-encode scheme (lambda (c)
                       (encode-params c params)
                       (cur-u16 c pid-sentinel)
                       (cur-u16 c 0))))

(define (payload->param-list payload)
  (cdr-decode payload decode-params))

;; ------------------------------------------------------------------
;; Sequence numbers

;; SequenceNumber: high (i32) + low (u32) in submessage endianness
;; [9.4.2.4].
(define (sn->cursor c sn)
  (unless (and (exact-nonnegative-integer? sn) (< sn (arithmetic-shift 1 64)))
    (raise-argument-error 'sn->cursor "sequence number" sn))
  (cur-i32 c (arithmetic-shift sn -32))
  (cur-u32 c (bitwise-and sn #xFFFFFFFF)))

(define (cursor-sn r)
  (define high (rd-i32 r))
  (define low (rd-u32 r))
  (unless (>= high 0)
    (malformed "negative sequence number high word"))
  (+ low (arithmetic-shift high 32)))

;; SequenceNumberSet [9.4.2.6]: bitmap base (SN), numBits (u32, 0 <
;; numBits <= 256), ceil(numBits/32) bitmap words. A set bit marks a
;; missing sequence number starting at base+1.
(struct seqnumset (base num-bits words) #:transparent)

(define (seqnumset->cursor c s)
  (sn->cursor c (seqnumset-base s))
  (define nb (seqnumset-num-bits s))
  ;; numBits 0 (empty state, e.g. a final ACKNACK) is legal
  (unless (and (exact-nonnegative-integer? nb) (<= nb 256))
    (raise-argument-error 'seqnumset->cursor "numBits in 0..256" nb))
  (define words (seqnumset-words s))
  (unless (= (length words) (ceiling (/ nb 32)))
    (raise-argument-error 'seqnumset->cursor
                          "one word per 32 bits" words))
  (cur-u32 c nb)
  (for ([w words]) (cur-u32 c w)))

(define (cursor-seqnumset r)
  (define base (cursor-sn r))
  (define nb (rd-u32 r))
  (unless (<= nb 256)
    (malformed "SequenceNumberSet numBits ~a exceeds 256" nb))
  (define nlongs (ceiling (/ nb 32)))
  (when (> nlongs (quotient (rd-remaining r) 4))
    (malformed "SequenceNumberSet bitmap overruns the submessage"))
  (define words
    (for/list ([_i (in-range nlongs)]) (rd-u32 r)))
  (seqnumset base nb words))

;; ------------------------------------------------------------------
;; Submessages

(struct info-ts (time invalidate?) #:transparent)      ; time = seconds
(struct info-dst (prefix) #:transparent)               ; prefix = 12 octets
(struct data-sub (reader-id writer-id sn qos payload invalidate?) #:transparent)
(struct heartbeat-sub (reader-id writer-id first-sn last-sn count final? liveliness?) #:transparent)
(struct acknack-sub (reader-id writer-id reader-sn-state count final?) #:transparent)

;; GAP has no struct of its own yet: it appears only in reliable
;; exchanges, which arrive with the P2 reliability layer.

;; Submessage kinds [9.4.5.3.1, table 9.15]
(define kind-pad #x01)
(define kind-acknack #x06)
(define kind-info-ts #x09)
(define kind-info-src #x0c)
(define kind-info-dst #x0e)
(define kind-data #x15)
(define kind-gap #x18)
(define kind-heartbeat #x1c)

;; flags
(define flag-little-endian #x01)
(define flag-data-invalidate #x02)
(define flag-group-info #x02)          ; GAP
(define flag-qos #x04)
(define flag-final #x04)               ; HEARTBEAT/ACKNACK
(define flag-data #x08)
(define flag-liveliness #x08)          ; HEARTBEAT

;; submessage content endianness; RTPS defaults to big endian, but the
;; ecosystem (Cyclone, Fast-DDS) speaks little endian in practice, so
;; this stack defaults to LE and can be flipped per message.
(define current-submessage-le? (make-parameter #t))

(define (encode-submessage c kind flags content-length [last? #f])
  ;; c carries the header endianness (big), so the count octets are
  ;; written by hand in the SUBMESSAGE endianness (flags bit 0)
  (define le? (not (zero? (bitwise-and flags flag-little-endian))))
  (define o2nh (if last? 0 content-length))
  (if le?
      (begin (cur-u8 c kind)
             (cur-u8 c flags)
             (cur-u8 c (bitwise-and o2nh #xFF))
             (cur-u8 c (bitwise-and (arithmetic-shift o2nh -8) #xFF)))
      (begin (cur-u8 c kind)
             (cur-u8 c flags)
             (cur-u8 c (bitwise-and (arithmetic-shift o2nh -8) #xFF))
             (cur-u8 c (bitwise-and o2nh #xFF)))))

(define (encode-data-sub c d le? last?)
  (define qos (data-sub-qos d))
  (define payload (data-sub-payload d))
  (define qos? (and qos (not (null? qos))))
  (define payload? (and payload (not (equal? payload #""))))
  (define flags
    (bitwise-ior (if le? flag-little-endian 0)
                 (if (data-sub-invalidate? d) flag-data-invalidate 0)
                 (if qos? flag-qos 0)
                 (if payload? flag-data 0)))
  ;; content: extraFlags(2) + octetsToInlineQos(2) + reader(4) +
  ;; writer(4) + SN(8) + qos + payload; measure the whole content by
  ;; encoding it into a scratch cursor
  (define body (make-cursor #:le? le?))
  (cur-u16 body 0) ; extraFlags
  (cur-u16 body (+ 4 4 8)) ; octetsToInlineQos: SN ends where QoS starts
  (cur-raw body (data-sub-reader-id d))
  (cur-raw body (data-sub-writer-id d))
  (sn->cursor body (data-sub-sn d))
  ;; inlineQos is delimited by a sentinel so a following payload has a
  ;; defined start; without payload the sentinel still ends the list
  (when qos?
    (encode-params body qos)
    (cur-u16 body pid-sentinel)
    (cur-u16 body 0))
  (when payload? (cur-raw body payload))
  (define content (cursor->bytes body))
  (encode-submessage c kind-data flags (bytes-length content) last?)
  (cur-raw c content))

(define (encode-heartbeat-sub c h le? last?)
  (define flags
    (bitwise-ior (if le? flag-little-endian 0)
                 (if (heartbeat-sub-final? h) flag-final 0)
                 (if (heartbeat-sub-liveliness? h) flag-liveliness 0)))
  (define body (make-cursor #:le? le?))
  (cur-raw body (heartbeat-sub-reader-id h))
  (cur-raw body (heartbeat-sub-writer-id h))
  (sn->cursor body (heartbeat-sub-first-sn h))
  (sn->cursor body (heartbeat-sub-last-sn h))
  (cur-u32 body (heartbeat-sub-count h))
  (define content (cursor->bytes body))
  (encode-submessage c kind-heartbeat flags (bytes-length content) last?)
  (cur-raw c content))

(define (encode-acknack-sub c a le? last?)
  (define flags
    (bitwise-ior (if le? flag-little-endian 0)
                 (if (acknack-sub-final? a) flag-final 0)))
  (define body (make-cursor #:le? le?))
  (cur-raw body (acknack-sub-reader-id a))
  (cur-raw body (acknack-sub-writer-id a))
  (seqnumset->cursor body (acknack-sub-reader-sn-state a))
  (cur-u32 body (acknack-sub-count a))
  (define content (cursor->bytes body))
  (encode-submessage c kind-acknack flags (bytes-length content) last?)
  (cur-raw c content))

(define (encode-gap-sub c reader-id writer-id gap-start gap-list le? last?)
  ;; group info (G flag) is not produced by this stack
  (define flags (if le? flag-little-endian 0))
  (define body (make-cursor #:le? le?))
  (cur-raw body reader-id)
  (cur-raw body writer-id)
  (sn->cursor body gap-start)
  (seqnumset->cursor body gap-list)
  (define content (cursor->bytes body))
  (encode-submessage c kind-gap flags (bytes-length content) last?)
  (cur-raw c content))

(define (encode-info-ts-sub c ts le? last?)
  (define flags
    (bitwise-ior (if le? flag-little-endian 0)
                 (if (info-ts-invalidate? ts) flag-data-invalidate 0)))
  (define body (make-cursor #:le? le?))
  (unless (info-ts-invalidate? ts)
    ;; Time_t: seconds (i32) + fraction of a second in 1/2^32 units (u32)
    (define frac (inexact->exact (floor (* (info-ts-time ts) 4294967296))))
    (cur-i32 body (arithmetic-shift frac -32))
    (cur-u32 body (bitwise-and frac #xFFFFFFFF)))
  (define content (cursor->bytes body))
  (encode-submessage c kind-info-ts flags (bytes-length content) last?)
  (cur-raw c content))

(define (encode-info-dst-sub c d le? last?)
  (define flags (if le? flag-little-endian 0))
  (check-prefix "info-dst prefix" (info-dst-prefix d))
  (encode-submessage c kind-info-dst flags 12 last?)
  (cur-raw c (info-dst-prefix d)))

;; Assemble a full RTPS message: header + submessages, the last one
;; carrying octetsToNextHeader = 0.
(define (encode-rtps-message header subs [le? (current-submessage-le?)])
  (define c (make-cursor #:le? #f)) ; header is always big endian
  (cur-raw c #"RTPS")
  (cur-u8 c (rtps-header-major header))
  (cur-u8 c (rtps-header-minor header))
  (cur-raw c (rtps-header-vendor-id header))
  (check-prefix "rtps-header prefix" (rtps-header-guid-prefix header))
  (cur-raw c (rtps-header-guid-prefix header))
  (let loop ([rest subs])
    (match rest
      ['() (void)]
      [(list sub) (encode-sub c sub le? #t)]
      [(cons sub more) (encode-sub c sub le? #f) (loop more)]))
  (cursor->bytes c))

(define (encode-sub c sub le? last?)
  (cond
    [(data-sub? sub) (encode-data-sub c sub le? last?)]
    [(heartbeat-sub? sub) (encode-heartbeat-sub c sub le? last?)]
    [(acknack-sub? sub) (encode-acknack-sub c sub le? last?)]
    [(info-ts? sub) (encode-info-ts-sub c sub le? last?)]
    [(info-dst? sub) (encode-info-dst-sub c sub le? last?)]
    [(and (list? sub) (= 5 (length sub)))
     ;; (list 'gap reader-id writer-id gap-start gap-list) shorthand
     (apply encode-gap-sub c (append (cdr sub) (list le? last?)))]
    [else (raise-argument-error 'encode-rtps-message "submessage" sub)]))

;; ------------------------------------------------------------------
;; Decoding

;; Tolerant decode: unknown submessage kinds are skipped via
;; octetsToNextHeader, unknown parameters are skipped by length.
;; Returns (values rtps-header submessage-list); wire-level problems
;; raise exn:fail:dds:malformed.
(define (decode-rtps-message bs)
  (unless (and (bytes? bs) (>= (bytes-length bs) 20))
    (malformed "message shorter than the 20-octet RTPS header"))
  (unless (equal? (subbytes bs 0 4) #"RTPS")
    (malformed "bad RTPS magic ~v" (subbytes bs 0 4)))
  (define major (bytes-ref bs 4))
  (define minor (bytes-ref bs 5))
  (unless (= major 2)
    (malformed "unsupported protocol major version ~a" major))
  (define vendor-id (subbytes bs 6 8))
  (define prefix (subbytes bs 8 20))
  (define header (rtps-header major minor vendor-id prefix))
  (define subs
    (let loop ([pos 20] [acc '()])
      (if (> (+ pos 4) (bytes-length bs))
          (reverse acc)
          (let* ([kind (bytes-ref bs pos)]
                 [flags (bytes-ref bs (add1 pos))]
                 [le? (= 1 (bitwise-and flags flag-little-endian))]
                 ;; octetsToNextHeader follows the submessage endianness
                 [o2nh (if le?
                           (bytes->u16le bs (+ pos 2))
                           (bytes->u16 bs (+ pos 2)))]
                 [content-start (+ pos 4)]
                 [content-len
                  (if (zero? o2nh)
                      (- (bytes-length bs) content-start)
                      o2nh)]
                 [content-end (+ content-start content-len)])
            (cond
              [(> content-end (bytes-length bs))
               (malformed "submessage 0x~x overruns the datagram" kind)]
              [(zero? o2nh)
               (define sub (decode-sub kind flags le? bs content-start content-end))
               (reverse (if sub (cons sub acc) acc))]
              [else
               (define sub (decode-sub kind flags le? bs content-start content-end))
               (loop content-end (if sub (cons sub acc) acc))])))))
  (values header subs))

(define (decode-sub kind flags le? bs start end)
  (define r (make-reader bs start end le?))
  (case kind
    [(#x15) (decode-data flags r)]
    [(#x1c) (decode-heartbeat flags r)]
    [(#x06) (decode-acknack flags r)]
    [(#x09) (decode-info-ts flags r)]
    [(#x0e) (decode-info-dst r)]
    [(#x18)
     ;; GAP: reader/writer/gapStart/gapList, optional group info
     ;; (G flag) which this stack reads but does not interpret
     (define reader-id (rd-raw r 4))
     (define writer-id (rd-raw r 4))
     (define gap-start (cursor-sn r))
     (define gap-list (cursor-seqnumset r))
     `(gap ,reader-id ,writer-id ,gap-start ,gap-list)]
    [else
     (dds-log-debug "message: skipping unknown submessage kind 0x~x" kind)
     #f]))

(define (decode-data flags r)
  (when (>= (rd-remaining r) 4)
    (let* ([_extra (rd-u16 r)]
           [o2inline-qos (rd-u16 r)]
           [reader-id (rd-raw r 4)]
           [writer-id (rd-raw r 4)]
           [sn (cursor-sn r)]
           [qos-present? (not (zero? (bitwise-and flags flag-qos)))]
           [payload-present? (not (zero? (bitwise-and flags flag-data)))]
           [invalidate? (not (zero? (bitwise-and flags flag-data-invalidate)))]
           ;; inlineQos entries are self-delimiting up to a sentinel;
           ;; the payload (if any) is whatever follows
           [qos (if qos-present? (decode-params r) '())]
           [payload (if payload-present?
                        (rd-raw r (rd-remaining r))
                        #f)])
      (data-sub reader-id writer-id sn qos payload invalidate?))))

(define (decode-heartbeat flags r)
  (when (>= (rd-remaining r) 28)
    (define reader-id (rd-raw r 4))
    (define writer-id (rd-raw r 4))
    (define first-sn (cursor-sn r))
    (define last-sn (cursor-sn r))
    (define count (rd-u32 r))
    (heartbeat-sub reader-id writer-id first-sn last-sn count
                   (not (zero? (bitwise-and flags flag-final)))
                   (not (zero? (bitwise-and flags flag-liveliness))))))

(define (decode-acknack flags r)
  (when (>= (rd-remaining r) 12)
    (define reader-id (rd-raw r 4))
    (define writer-id (rd-raw r 4))
    (define state (cursor-seqnumset r))
    (define count (rd-u32 r))
    (acknack-sub reader-id writer-id state count
                 (not (zero? (bitwise-and flags flag-final))))))

(define (decode-info-ts flags r)
  (if (not (zero? (bitwise-and flags flag-data-invalidate)))
      (info-ts #f #t)
      (when (>= (rd-remaining r) 8)
        (define secs (rd-i32 r))
        (define frac (rd-u32 r))
        (info-ts (+ secs (/ frac 4294967296.0)) #f))))

(define (decode-info-dst r)
  (when (>= (rd-remaining r) 12)
    (info-dst (rd-raw r 12))))

;; ------------------------------------------------------------------
;; Tests

(module+ test
  (define prefix (make-bytes 12 #x11))
  (define vendor (bytes #x01 #xF0))

  ;; header golden bytes [9.4.4.4]
  (define header (rtps-header 2 3 vendor prefix))
  (define hdr-bytes
    (bytes-append #"RTPS" (bytes 2 3) vendor prefix))
  (check-equal? (subbytes (encode-rtps-message header '()) 0 20) hdr-bytes)
  (define-values (h0 s0) (decode-rtps-message hdr-bytes))
  (check-equal? h0 header)
  (check-equal? s0 '())

  ;; DATA golden bytes: no QoS, no payload, writer = SPDP, SN 1, LE
  ;; (content is 20 octets; octetsToInlineQos is the 16 from readerId
  ;; to inlineQos, octetsToNextHeader counts the whole content)
  (define d (data-sub entity-id-unknown entity-id-spdp-participant 1 '() #f #f))
  (define data-bytes
    (bytes-append hdr-bytes
                  (bytes #x15 #x01 #x14 #x00)      ; kind, flags(LE), o2nh=20
                  (bytes 0 0)                      ; extraFlags
                  (bytes #x10 #x00)                ; octetsToInlineQos = 16
                  entity-id-unknown
                  entity-id-spdp-participant
                  (bytes 0 0 0 0 1 0 0 0)))        ; SN 1: high 0, low 1 LE
  (define-values (h1 s1) (decode-rtps-message data-bytes))
  (check-equal? h1 header)
  (check-equal? s1 (list d) "DATA round trip with golden bytes")
  (check-equal? (data-sub-reader-id (car s1)) entity-id-unknown)
  (check-equal? (data-sub-sn (car s1)) 1)

  ;; big-endian mirror: header, o2nh, octetsToInlineQos and SN all
  ;; big endian (o2nh 20 = 0x0014, inlineQos offset 16 = 0x0010)
  (define-values (_h2 s2)
    (decode-rtps-message
     (bytes-append hdr-bytes
                   (bytes #x15 #x00 #x00 #x14)
                   (bytes 0 0)
                   (bytes #x00 #x10)
                   entity-id-unknown entity-id-spdp-participant
                   (bytes 0 0 0 0 0 0 0 1)))) ; SN 1 big endian
  (check-equal? (data-sub-sn (car s2)) 1 "SN decodes per submessage endianness")

  ;; parameter list golden bytes (PL_CDR_LE payload)
  (define params
    (list (param-string pid-topic-name "dcpsparticipant")))
  (define payload (param-list->payload params pl-cdr-le))
  (check-equal? payload
                (bytes-append (bytes #x00 #x03 #x00 #x00) ; PL_CDR_LE header
                              (bytes #x05 #x00 #x0F #x00) ; PID, len 15
                              #"dcpsparticipant"
                              (bytes 0)                    ; pad to 4
                              (bytes #x01 #x00 #x00 #x00)) ; sentinel
                "param list wire form with sentinel and padding")
  (define-values (back scheme _pl-opts) (payload->param-list payload))
  (check-equal? scheme 'pl-cdr-le)
  (check-equal? back params)
  (check-equal? (param->string (car back)) "dcpsparticipant")

  ;; locator parameter round trip
  (define loc-param (param-locator pid-default-unicast-locator
                                   (udpv4-locator "192.168.1.5" 7401)))
  (define-values (loc-back _s3 _o3) (payload->param-list (param-list->payload (list loc-param))))
  (check-true (locator=? (param->locator (car loc-back)) (udpv4-locator "192.168.1.5" 7401)))

  ;; sequence number set golden bytes
  (define sns (seqnumset 10 32 (list #x80000001)))
  (define c (make-cursor #:le? #t))
  (seqnumset->cursor c sns)
  (check-equal? (cursor->bytes c)
                (bytes 0 0 0 0 10 0 0 0   ; base 10 LE
                       32 0 0 0           ; numBits
                       1 0 0 #x80)        ; word 0x80000001 LE
                "SequenceNumberSet wire form")
  (check-equal? (let ([r (make-reader (cursor->bytes c) 0 (cursor-size c) #t)])
                  (cursor-seqnumset r))
                sns)

  ;; DATA with inline QoS (KEY_HASH + STATUS_INFO) and PL payload,
  ;; the shape of an SPDP announcement
  (define guid-b (make-guid prefix entity-id-spdp-participant))
  (define spdp-data
    (data-sub entity-id-unknown entity-id-spdp-participant 7
              (list (param-guid pid-key-hash guid-b)
                    status-info-alive)
              payload
              #f))
  (define spdp-msg (encode-rtps-message header (list spdp-data)))
  (define-values (h4 s4) (decode-rtps-message spdp-msg))
  (check-equal? s4 (list spdp-data) "SPDP-shaped DATA round trip")
  (check-equal? (param->guid (car (data-sub-qos (car s4)))) guid-b)
  (check-equal? (data-sub-payload (car s4)) payload)

  ;; HEARTBEAT round trip
  (define hb (heartbeat-sub entity-id-unknown entity-id-spdp-participant
                            1 5 3 #t #f))
  (define-values (_h5 s5) (decode-rtps-message (encode-rtps-message header (list hb))))
  (check-equal? s5 (list hb))

  ;; ACKNACK round trip
  (define an (acknack-sub entity-id-spdp-participant entity-id-spdp-participant
                          (seqnumset 3 16 (list 5)) 2 #f))
  (define-values (_h6 s6) (decode-rtps-message (encode-rtps-message header (list an))))
  (check-equal? s6 (list an))

  ;; INFO_TS + INFO_DST round trip
  (define ts (info-ts 1234.5 #f))
  (define dst (info-dst prefix))
  (define-values (_h7 s7)
    (decode-rtps-message (encode-rtps-message header (list ts dst hb))))
  (check-equal? (length s7) 3)
  (check-true (< (abs (- (info-ts-time (car s7)) 1234.5)) 1e-6))
  (check-equal? (info-dst-prefix (cadr s7)) prefix)
  (check-equal? (caddr s7) hb)

  ;; unknown submessage kinds are skipped via octetsToNextHeader
  (define unknown-sub
    (bytes-append (bytes #x42 #x01 #x08 #x00) ; kind 0x42, LE, 8 octets
                  (make-bytes 8 #xAA)
                  (bytes #x15 #x01 #x14 #x00)
                  (bytes 0 0) (bytes #x10 #x00)
                  entity-id-unknown entity-id-spdp-participant
                  (bytes 0 0 0 0 1 0 0 0)))
  (define-values (_h8 s8) (decode-rtps-message (bytes-append hdr-bytes unknown-sub)))
  (check-equal? (length s8) 1)

  ;; status info round trip
  (check-equal? (call-with-values
                 (lambda () (param->status-info status-info-disposed)) list)
                (list #t 1))
  (check-equal? (call-with-values
                 (lambda () (param->status-info status-info-alive)) list)
                (list #f 0))

  ;; malformed messages
  (check-exn exn:fail:dds:malformed? (lambda () (decode-rtps-message #"NOPE")))
  (check-exn exn:fail:dds:malformed?
             (lambda ()
               (decode-rtps-message
                (bytes-append hdr-bytes
                              (bytes #x15 #x01 #xFF #xFF) ; o2nh overruns
                              (make-bytes 4)))))
  ;; a SequenceNumberSet with numBits out of range is refused on encode
  (check-exn exn:fail?
             (lambda ()
               (encode-rtps-message
                header
                (list (acknack-sub entity-id-unknown entity-id-unknown
                                   (seqnumset 0 999 (list 1)) 0 #f))))))
