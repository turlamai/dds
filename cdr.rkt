#lang racket/base

;; CDR / XCDR1 value codec (DDS-RTPS 2.3, chapter 7.4 "CDR transfer
;; format").
;;
;; Wire rules implemented here:
;;   [7.4.1]   every CDR payload starts with a 4-octet encapsulation
;;             header: 2-octet scheme identifier + 2-octet options
;;             (options are usually 0)
;;   [7.4.2.1] primitives are aligned to their natural size (1/2/4/8)
;;             measured from the START of the serialized stream, which
;;             includes the encapsulation header - the classic CDR
;;             pitfall: padding is not relative to the first payload
;;             octet after the header
;;   [7.4.4]   strings carry a 4-octet length including the terminating
;;             NUL, then the octets and the NUL itself; no padding
;;             behind the NUL (the next member aligns as usual)
;;   [7.4.4.1] sequences carry a 4-octet element count, then the
;;             elements, each aligned to its natural size
;;   [9.4.2]   ParameterList values (discovery data) are encoded with
;;             the PL_CDR encapsulations; each parameter is
;;             PID (2) + length (2) + value padded to a multiple of 4
;;
;; The module exposes two halves: an encode half driven by a cursor
;; (primitives align and write), and a decode half driven by a reader
;; (primitives align-skip and read). Alignment on decode uses the
;; absolute reader position, which is the stream position because
;; callers always hand in the whole payload byte string.

(require racket/contract
         racket/format
         racket/match
         "private/bytes.rkt"
         "logging.rkt")

(provide (struct-out exn:fail:dds:malformed)
         malformed
         cdr-be cdr-le pl-cdr-be pl-cdr-le
         encapsulation? encapsulation->symbol symbol->encapsulation
         cdr-encode cdr-decode
         cdr-align! cdr-align-skip
         cdr-u8 cdr-u16 cdr-u32 cdr-u64
         cdr-i8 cdr-i16 cdr-i32 cdr-i64
         cdr-f32 cdr-f64
         cdr-boolean cdr-octets cdr-string
         cdr-seq cdr-octet-seq
         cdr-read-u8 cdr-read-u16 cdr-read-u32 cdr-read-u64
         cdr-read-i8 cdr-read-i16 cdr-read-i32 cdr-read-i64
         cdr-read-f32 cdr-read-f64
         cdr-read-boolean cdr-read-octets cdr-read-string
         cdr-read-seq cdr-read-octet-seq)

(module+ test (require rackunit))

;; Shared exception hierarchy for the whole stack, mirroring the
;; someip package: wire-level decoding problems raise
;; exn:fail:dds:malformed.
(define (malformed fmt . args)
  (raise (exn:fail:dds:malformed (apply format (string-append "dds-cdr: " fmt) args)
                                 (current-continuation-marks))))

(struct exn:fail:dds:malformed exn:fail ())

;; ------------------------------------------------------------------
;; Encapsulation schemes (DDS-RTPS 2.3, table 7.4)

(define cdr-be     0) ; 0x0000 CDR_BE
(define cdr-le     1) ; 0x0001 CDR_LE
(define pl-cdr-be  2) ; 0x0002 PL_CDR_BE (ParameterList)
(define pl-cdr-le  3) ; 0x0003 PL_CDR_LE (ParameterList)

(define (encapsulation? v) (and (exact-integer? v) (<= 0 v 3)))

(define (encapsulation->symbol e)
  (match e
    [0 'cdr-be] [1 'cdr-le] [2 'pl-cdr-be] [3 'pl-cdr-le]
    [_ (raise-argument-error 'encapsulation->symbol "encapsulation?" e)]))

(define (symbol->encapsulation s)
  (match s
    ['cdr-be cdr-be] ['cdr-le cdr-le]
    ['pl-cdr-be pl-cdr-be] ['pl-cdr-le pl-cdr-le]
    [_ (raise-argument-error 'symbol->encapsulation
                             "'cdr-be/'cdr-le/'pl-cdr-be/'pl-cdr-le" s)]))

;; ------------------------------------------------------------------
;; Alignment

;; Pad the cursor so the next write starts on an n-octet boundary,
;; counted from stream position 0. Per [7.4.2.1] this INCLUDES the
;; encapsulation header, so callers encode payloads from position 4
;; (right after `cdr-encode` wrote the header) and the math stays
;; stream-absolute.
(define (cdr-align! c n)
  (define pos (cursor-pos c))
  (define pad (modulo (- n (modulo pos n)) n))
  (when (positive? pad) (cur-fill c pad)))

;; Advance the reader past the same padding, using the absolute reader
;; position as the stream offset.
(define (cdr-align-skip r n)
  (define pos (reader-pos r))
  (define pad (modulo (- n (modulo pos n)) n))
  (rd-skip r pad))

;; ------------------------------------------------------------------
;; Encoding (cursor half)

(define (cdr-u8 c v) (cur-u8 c v))
(define (cdr-i8 c v) (cur-i8 c v))

(define (cdr-u16 c v) (cdr-align! c 2) (cur-u16 c v))
(define (cdr-i16 c v) (cdr-align! c 2) (cur-i16 c v))
(define (cdr-u32 c v) (cdr-align! c 4) (cur-u32 c v))
(define (cdr-i32 c v) (cdr-align! c 4) (cur-i32 c v))
(define (cdr-u64 c v) (cdr-align! c 8) (cur-u64 c v))
(define (cdr-i64 c v) (cdr-align! c 8) (cur-i64 c v))
(define (cdr-f32 c v) (cdr-align! c 4) (cur-f32 c v))
(define (cdr-f64 c v) (cdr-align! c 8) (cur-f64 c v))

(define (cdr-boolean c v)
  (cur-u8 c (if v 1 0)))

;; Fixed octet run (member of an octet array); caller aligns if a
;; primitive follows.
(define (cdr-octets c bs) (cur-raw c bs))

;; String: 4-octet length INCLUDING the NUL, then octets + NUL
;; [7.4.4]. Accepts byte strings or text; text is written UTF-8
;; (DDS `char` strings in the discovery profile are ASCII in practice).
(define (cdr-string c s)
  (define bs (if (string? s) (string->bytes/utf-8 s) s))
  (cdr-u32 c (add1 (bytes-length bs)))
  (cur-raw c bs)
  (cur-u8 c 0))

;; Sequence of elements [7.4.4.1]: 4-octet count, then each element via
;; `element-enc!`, which must do its own alignment.
(define (cdr-seq c vs element-enc!)
  (cdr-u32 c (length vs))
  (for ([v vs]) (element-enc! c v)))

;; Sequence of octets: count + raw bytes.
(define (cdr-octet-seq c bs)
  (cdr-u32 c (bytes-length bs))
  (cur-raw c bs))

;; ------------------------------------------------------------------
;; Decoding (reader half)

(define (cdr-read-u8 r) (rd-u8 r))
(define (cdr-read-i8 r) (rd-i8 r))

(define (cdr-read-u16 r) (cdr-align-skip r 2) (rd-u16 r))
(define (cdr-read-i16 r) (cdr-align-skip r 2) (rd-i16 r))
(define (cdr-read-u32 r) (cdr-align-skip r 4) (rd-u32 r))
(define (cdr-read-i32 r) (cdr-align-skip r 4) (rd-i32 r))
(define (cdr-read-u64 r) (cdr-align-skip r 8) (rd-u64 r))
(define (cdr-read-i64 r) (cdr-align-skip r 8) (rd-i64 r))
(define (cdr-read-f32 r) (cdr-align-skip r 4) (rd-f32 r))
(define (cdr-read-f64 r) (cdr-align-skip r 8) (rd-f64 r))

(define (cdr-read-boolean r)
  (case (rd-u8 r) [(0) #f] [(1) #t] [else (malformed "boolean octet is not 0/1")]))

(define (cdr-read-octets r n) (rd-raw r n))

(define (cdr-read-string r)
  (define len (cdr-read-u32 r))
  (unless (>= len 1) (malformed "string length ~a excludes the NUL" len))
  (define bs (rd-raw r len))
  (unless (zero? (bytes-ref bs (sub1 len)))
    (malformed "string is not NUL terminated"))
  (subbytes bs 0 (sub1 len)))

(define (cdr-read-seq r element-dec!)
  (define count (cdr-read-u32 r))
  (when (> count (rd-remaining r))
    (malformed "sequence count ~a larger than remaining bytes" count))
  (for/list ([_i (in-range count)]) (element-dec! r)))

(define (cdr-read-octet-seq r)
  (define count (cdr-read-u32 r))
  (when (> count (rd-remaining r))
    (malformed "sequence count ~a larger than remaining bytes" count))
  (rd-raw r count))

;; ------------------------------------------------------------------
;; Encapsulation wrappers

;; Encode a CDR payload: write the encapsulation header, then run
;; `write!` on a cursor positioned right after it (stream position 4).
;; Returns the full payload byte string.
(define (cdr-encode scheme write!)
  (unless (encapsulation? scheme)
    (raise-argument-error 'cdr-encode "encapsulation?" scheme))
  (define le? (odd? scheme)) ; CDR_LE=1, PL_CDR_LE=3 are little endian
  (define c (make-cursor #:le? le?))
  ;; the encapsulation header is a fixed octet pair (0x00, scheme)
  ;; followed by a zero options pair; it does NOT follow the payload
  ;; byte order
  (cur-u8 c 0)
  (cur-u8 c scheme)
  (cur-fill c 2) ; options
  (write! c)
  (cursor->bytes c))

;; Decode a CDR payload: read scheme + options, then run `read!` on a
;; reader positioned right after the header. Returns
;; (values result encapsulation-symbol options).
(define (cdr-decode payload read!)
  (unless (and (bytes? payload) (>= (bytes-length payload) 4))
    (malformed "CDR payload shorter than the 4-octet encapsulation header"))
  (define scheme (bytes->u16 payload 0))
  (define options (bytes->u16 payload 2))
  (unless (encapsulation? scheme)
    (malformed "unknown CDR encapsulation scheme 0x~x" scheme))
  (define le? (odd? scheme))
  (define r (make-reader payload 4 (bytes-length payload) le?))
  (values (read! r) (encapsulation->symbol scheme) options))

;; ------------------------------------------------------------------
;; Tests

(module+ test
  (define (enc th) (cdr-encode cdr-le th))
  (define (dec bs th)
    (call-with-values (lambda () (cdr-decode bs th)) list))

  (check-equal? (enc (lambda (c) (cdr-u32 c 1)))
                (bytes #x00 #x01 #x00 #x00 1 0 0 0)
                "u32 after the encapsulation header is already aligned")
  (check-equal? (enc (lambda (c) (cdr-u8 c 7) (cdr-u16 c 8)))
                (bytes #x00 #x01 #x00 #x00 7 0 8 0)
                "u16 pads one octet after a u8, counted from stream start")
  (check-equal? (enc (lambda (c) (cdr-u64 c 1) (cdr-u8 c 2) (cdr-u64 c 3)))
                (bytes #x00 #x01 #x00 #x00
                       0 0 0 0           ; pad 4: u64 aligns to stream offset 8
                       1 0 0 0 0 0 0 0   ; u64 1 at offset 8
                       2                 ; u8 at offset 16
                       0 0 0 0 0 0 0     ; pad 7: next u64 at stream offset 24
                       3 0 0 0 0 0 0 0)  ; u64 3 at offset 24
                "u64 realigns to a stream-relative boundary")
  (check-equal? (enc (lambda (c) (cdr-f64 c 1.0)))
                (bytes #x00 #x01 #x00 #x00
                       0 0 0 0                  ; pad to stream offset 8
                       #x00 #x00 #x00 #x00 #x00 #x00 #xF0 #x3F)
                "f64 aligns to 8 and encodes little endian")

  (check-equal? (enc (lambda (c) (cdr-string c "ab")))
                (bytes #x00 #x01 #x00 #x00 3 0 0 0 #x61 #x62 #x00)
                "string length includes the NUL")
  (check-equal? (enc (lambda (c) (cdr-string c #"xy")))
                (bytes #x00 #x01 #x00 #x00 3 0 0 0 #x78 #x79 #x00))

  (check-equal? (enc (lambda (c) (cdr-octet-seq c (bytes 1 2))))
                (bytes #x00 #x01 #x00 #x00 2 0 0 0 1 2))
  (check-equal? (enc (lambda (c) (cdr-seq c '(1 2) (lambda (c v) (cdr-u16 c v)))))
                (bytes #x00 #x01 #x00 #x00 2 0 0 0 1 0 2 0)
                "sequence elements align individually")

  ;; big endian mirror
  (check-equal? (cdr-encode cdr-be (lambda (c) (cdr-u32 c 1)))
                (bytes #x00 #x00 #x00 #x00 0 0 0 1))

  ;; decode round trips
  (check-equal?
   (dec (enc (lambda (c) (cdr-u8 c 7) (cdr-u64 c 8) (cdr-string c "hi")
             (cdr-seq c '(5 6) (lambda (c v) (cdr-u32 c v)))
             (cdr-boolean c #t) (cdr-f32 c 2.5) (cdr-f64 c 3.5)))
        (lambda (r)
          (list (cdr-read-u8 r) (cdr-read-u64 r) (bytes->string/utf-8 (cdr-read-string r))
                (cdr-read-seq r cdr-read-u32)
                (cdr-read-boolean r) (cdr-read-f32 r) (cdr-read-f64 r))))
   (list (list 7 8 "hi" '(5 6) #t 2.5 3.5) 'cdr-le 0)
   "decode round trip with realigned padding")

  (check-equal? (dec (enc (lambda (c) (cdr-u16 c 9))) cdr-read-u16)
                (list 9 'cdr-le 0))

  ;; malformed inputs
  (check-exn exn:fail:dds:malformed?
             (lambda () (cdr-decode (bytes 0 0) cdr-read-u16)))
  (check-exn exn:fail:dds:malformed?
             (lambda () (cdr-decode (bytes 0 9 0 0 1) cdr-read-u16)))
  (check-exn exn:fail:dds:malformed?
             (lambda ()
               ;; same string payload with the terminating NUL corrupted
               (cdr-decode (bytes #x00 #x01 #x00 #x00 3 0 0 0 #x61 #x62 #x41)
                           cdr-read-string)))
  (check-exn exn:fail:dds:malformed?
             (lambda ()
               ;; sequence count larger than the remaining bytes
               (cdr-decode (enc (lambda (c) (cdr-u32 c 999)))
                           (lambda (r) (cdr-read-seq r cdr-read-u8))))))
