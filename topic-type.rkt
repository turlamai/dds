#lang racket/base

;; Programmatic topic type DSL: first-class type descriptors that
;; generate CDR (XCDR1) codecs (DDS-RTPS 2.3, chapter 7.4 "CDR
;; transfer format"), in the same style as the someip package's
;; serialize.rkt: a type is a value with an encoder and a decoder,
;; composed into strings, arrays, sequences, structs and enums.
;;
;; Types describe ONE payload encoding; the byte order comes from the
;; encapsulation scheme at encode/decode time (cdr-encode/cdr-decode
;; flip the cursor/reader), and alignment is stream-relative per
;; [7.4.2.1] - both are handled by (dds cdr), so types stay
;; endianness-agnostic.
;;
;; Values on the Racket side:
;;   primitives  exact integers / flonums / booleans
;;   string-t    text strings (UTF-8 on the wire, NUL-terminated)
;;   octet-seq-t byte strings
;;   fixed-array / sequence-t  lists of element values
;;   struct-t    assoc lists '((member-name . value) ...), members in
;;               declaration order; unknown members are dropped on
;;               decode, missing members decode as #f
;;   enum-t      symbols mapped through the declaration table
;;               (32-bit discriminant on the wire)

(require racket/contract
         racket/dict
         racket/format
         racket/list
         racket/match
         racket/string
         "cdr.rkt"
         "logging.rkt")

(provide (struct-out dds-type)
         u8-t u16-t u32-t u64-t
         i8-t i16-t i32-t i64-t
         f32-t f64-t bool-t
         string-t octet-seq-t
         fixed-array sequence
         struct-member struct-t
         enum
         type-encode type-decode)

(module+ test (require rackunit racket/list))

;; A type descriptor: enc writes a value into a CDR cursor, dec reads
;; one back from a CDR reader, fixed-size is the wire size in bytes
;; for fixed-layout types or #f. Calling a type as a procedure encodes
;; a value under the default (little endian) scheme - a convenience
;; matching the someip codec style.
(struct dds-type (enc dec fixed-size name)
  #:property prop:procedure
  (lambda (t v)
    (type-encode t v 'cdr-le)))

;; ------------------------------------------------------------------
;; Primitives (alignment and endianness per [7.4.2.1] live in cdr.rkt)

(define (prim enc! dec! fixed name)
  (dds-type (lambda (c v) (enc! c v))
              (lambda (r) (dec! r))
              fixed name))

(define u8-t  (prim cdr-u8 cdr-read-u8 1 "u8"))
(define u16-t (prim cdr-u16 cdr-read-u16 2 "u16"))
(define u32-t (prim cdr-u32 cdr-read-u32 4 "u32"))
(define u64-t (prim cdr-u64 cdr-read-u64 8 "u64"))
(define i8-t  (prim cdr-i8 cdr-read-i8 1 "i8"))
(define i16-t (prim cdr-i16 cdr-read-i16 2 "i16"))
(define i32-t (prim cdr-i32 cdr-read-i32 4 "i32"))
(define i64-t (prim cdr-i64 cdr-read-i64 8 "i64"))
(define f32-t (prim cdr-f32 cdr-read-f32 4 "f32"))
(define f64-t (prim cdr-f64 cdr-read-f64 8 "f64"))
(define bool-t (prim cdr-boolean cdr-read-boolean 1 "bool"))

;; UTF-8 text; the wire form is the CDR bounded/unbounded string
;; (length includes the NUL) [7.4.4]
(define string-t
  (dds-type (lambda (c v) (cdr-string c (if (string? v) v (bytes->string/utf-8 v))))
              (lambda (r) (bytes->string/utf-8 (cdr-read-string r)))
              #f "string"))

;; Unbounded octet sequence: u32 count + raw octets [7.4.4.1]
(define octet-seq-t
  (dds-type (lambda (c v) (cdr-octet-seq c v))
              (lambda (r) (cdr-read-octet-seq r))
              #f "octet-seq"))

;; ------------------------------------------------------------------
;; Arrays and sequences

;; Fixed-length array: n elements, each aligned individually (same
;; layout as a sequence of the same element type, minus the count)
(define (fixed-array elem n [name #f])
  (unless (dds-type? elem)
    (raise-argument-error 'fixed-array "dds-type?" elem))
  (unless (exact-positive-integer? n)
    (raise-argument-error 'fixed-array "exact positive length" n))
  (dds-type
   (lambda (c vs)
     (define lst (if (list? vs) vs (vector->list vs)))
     (unless (= (length lst) n)
       (raise-argument-error (or name "fixed-array")
                             (format "array of ~a elements" n) vs))
     (for ([v lst]) ((dds-type-enc elem) c v)))
   (lambda (r)
     (for/list ([_i (in-range n)]) ((dds-type-dec elem) r)))
   (let ([ef (dds-type-fixed-size elem)])
     (and ef (* ef n)))
   (or name (format "array<~a,~a>" (dds-type-name elem) n))))

;; Bounded or unbounded sequence: u32 count + elements [7.4.4.1]
(define (sequence elem [max-length #f] [name #f])
  (unless (dds-type? elem)
    (raise-argument-error 'sequence "dds-type?" elem))
  (when max-length
    (unless (exact-positive-integer? max-length)
      (raise-argument-error 'sequence "exact positive max length or #f" max-length)))
  (dds-type
   (lambda (c vs)
     (define lst (if (list? vs) vs (vector->list vs)))
     (when (and max-length (> (length lst) max-length))
       (raise-argument-error (or name "sequence")
                             (format "sequence of at most ~a elements" max-length) vs))
     (cdr-seq c lst (dds-type-enc elem)))
   (lambda (r)
     (cdr-read-seq r (dds-type-dec elem)))
   #f
   (or name (format "seq<~a>" (dds-type-name elem)))))

;; ------------------------------------------------------------------
;; Structs and enums

;; One struct member: a name and a type. Declared with `struct`.
(struct struct-member (name type) #:transparent)

;; Struct type: members encoded in declaration order, each aligned
;; per its own type [7.4.2.1]. Values are assoc lists; decode keeps
;; declaration order and drops nothing (members absent from the wire
;; decode as #f only when the stream ran out - a short stream is a
;; malformed payload and raises there).
(define (struct-t . members)
  (for ([m members])
    (unless (struct-member? m)
      (raise-argument-error 'struct-t "struct-member?" m)))
  (define names (map struct-member-name members))
  (unless (= (length names) (length (remove-duplicates names)))
    (raise-argument-error 'struct-t "unique member names" names))
  (dds-type
   (lambda (c v)
     (unless (dict? v)
       (raise-argument-error 'struct-t-encode "assoc list (dict)" v))
     (for ([m members])
       (define val (dict-ref v (struct-member-name m)))
       (when (not val)
         (raise-argument-error 'struct-t-encode
                               (format "assoc list with member ~a"
                                       (struct-member-name m))
                               v))
       ((dds-type-enc (struct-member-type m)) c val)))
   (lambda (r)
     (for/list ([m members])
       (cons (struct-member-name m)
             ((dds-type-dec (struct-member-type m)) r))))
   (let ([sizes (map dds-type-fixed-size (map struct-member-type members))])
     (if (andmap values sizes) (apply + sizes) #f))
   (format "struct{~a}"
           (string-join (map (lambda (m) (symbol->string (struct-member-name m))) members) ","))))

;; Enum type: symbols mapped to 32-bit discriminants [7.4.2.1]
;; (enums ride on i32).
(define (enum name table)
  (unless (and (list? table) (pair? table))
    (raise-argument-error 'enum "non-empty (symbol code) list" table))
  (define by-sym (make-hash (for/list ([pair table]) (cons (car pair) (cdr pair)))))
  (define by-code (make-hash (for/list ([pair table]) (cons (cdr pair) (car pair)))))
  (for ([pair table])
    (unless (symbol? (car pair))
      (raise-argument-error 'enum "symbol key" pair))
    (unless (exact-integer? (cdr pair))
      (raise-argument-error 'enum "integer code" pair)))
  (dds-type
   (lambda (c v)
     (define code (hash-ref by-sym v #f))
     (unless code
       (raise-argument-error name (format "one of ~a" (map car table)) v))
     (cdr-i32 c code))
   (lambda (r)
     (define code (cdr-read-i32 r))
     (hash-ref by-code code code))
   4
   (format "enum(~a)" name)))

;; ------------------------------------------------------------------
;; Encode/decode entry points

;; Encode a value under an encapsulation scheme ('cdr-le/'cdr-be);
;; returns the full payload byte string including the encapsulation
;; header.
(define (type-encode t v [scheme 'cdr-le])
  (unless (dds-type? t)
    (raise-argument-error 'type-encode "dds-type?" t))
  (cdr-encode (symbol->encapsulation scheme)
              (lambda (c) ((dds-type-enc t) c v))))

;; Decode a payload; returns (values value scheme).
(define (type-decode t payload)
  (unless (dds-type? t)
    (raise-argument-error 'type-decode "dds-type?" t))
  (let-values ([(v scheme _opts)
                (cdr-decode payload (lambda (r) ((dds-type-dec t) r)))])
    (values v scheme)))

;; ------------------------------------------------------------------
;; Tests

(module+ test
  (define (rt t v)
    (let-values ([(back scheme) (type-decode t (type-encode t v))])
      (list back scheme)))

  ;; primitives round trip in both endiannesses
  (check-equal? (rt u32-t 4242) (list 4242 'cdr-le))
  (check-equal? (rt f64-t 1.5) (list 1.5 'cdr-le))
  (check-equal? (let ([bs (type-encode u32-t 1 'cdr-be)])
                  (list (bytes-ref bs 1) (bytes-ref bs 7)))
                (list 0 1)
                "cdr-be flips both header and payload")
  (check-equal? (rt bool-t #t) (list #t 'cdr-le))

  ;; strings and octet sequences
  (check-equal? (rt string-t "hello") (list "hello" 'cdr-le))
  (check-equal? (rt octet-seq-t (bytes 1 2 3)) (list (bytes 1 2 3) 'cdr-le))

  ;; fixed array golden bytes: three u16 elements, individually aligned
  (check-equal? (type-encode (fixed-array u16-t 3 "tri") '(1 2 3))
                (bytes 0 1 0 0          ; CDR_LE header
                       1 0 2 0 3 0)
                "u16 array packs without padding between elements")
  (check-equal? (rt (fixed-array u16-t 3) '(1 2 3)) (list '(1 2 3) 'cdr-le))
  (check-exn exn:fail? (lambda () (type-encode (fixed-array u16-t 3) '(1 2))))

  ;; sequence golden bytes
  (check-equal? (type-encode (sequence u16-t) '(1 2))
                (bytes 0 1 0 0
                       2 0 0 0        ; count
                       1 0 2 0)
                "sequence carries a u32 count")
  (check-equal? (rt (sequence u8-t) '(9 8 7)) (list '(9 8 7) 'cdr-le))
  (check-exn exn:fail? (lambda () (type-encode (sequence u8-t 2) '(1 2 3))
                             "bounded sequence refuses overflow"))

  ;; struct: u8 then u32 exercises stream-relative alignment inside
  ;; the payload (u32 lands on stream offset 8, so 3 pad octets)
  (define point
    (struct-t (struct-member 'flag u8-t)
            (struct-member 'x u32-t)
            (struct-member 'label string-t)))
  (check-equal? (type-encode point '((flag . 1) (x . 2) (label . "ab")))
                (bytes 0 1 0 0
                       1                ; flag at stream offset 4
                       0 0 0            ; pad: x aligns to stream offset 8
                       2 0 0 0          ; x
                       3 0 0 0          ; string length (2 + NUL)
                       #x61 #x62 0)     ; "ab\0"
                "struct members follow stream-relative CDR alignment")
  (check-equal? (rt point '((flag . 1) (x . 2) (label . "ab")))
                (list '((flag . 1) (x . 2) (label . "ab")) 'cdr-le))
  (check-exn exn:fail? (lambda () (type-encode point '((x . 2)))))

  ;; nested struct + enum + array composition
  (define kind-t (enum "kind" '((raw . 1) (cooked . 2))))
  (define dish
    (struct-t (struct-member 'kind kind-t)
            (struct-member 'temps (sequence f32-t))
            (struct-member 'pos (struct-t (struct-member 'lat f64-t)
                                        (struct-member 'lon f64-t)))))
  (define-values (back scheme)
    (type-decode dish (type-encode dish '((kind . cooked)
                                          (temps . (98.5 100.25))
                                          (pos . ((lat . 52.5) (lon . 13.4)))))))
  (check-equal? scheme 'cdr-le)
  (check-equal? (dict-ref back 'kind) 'cooked)
  (check-equal? (dict-ref back 'temps) (list 98.5 100.25)
                "f32 keeps values that are exact in binary")
  (check-equal? (dict-ref (dict-ref back 'pos) 'lat) 52.5)

  ;; calling a type encodes under the default scheme
  (check-equal? (u32-t 7) (type-encode u32-t 7 'cdr-le))

  ;; argument validation
  (check-exn exn:fail? (lambda () (fixed-array "nope" 3)))
  (check-exn exn:fail? (lambda () (sequence 42)))
  (check-exn exn:fail? (lambda () (enum "e" '())))
  (check-exn exn:fail? (lambda () (type-encode kind-t 'raw-plus)))
  ;; truncated payload raises from the byte layer (plain exn:fail)
  (check-exn exn:fail?
             (lambda () (type-decode u32-t (bytes 0 1 0 0 1 0)))))
