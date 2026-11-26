#lang info

(define name "dds")

(define collection "dds")

(define version "0.1")

;; The shipped collection depends on base alone: pure Racket, no foreign
;; libraries. rackunit-lib stays a runtime dep because test/ ships inside
;; this package and raco setup --check-pkg-deps classifies its requires
;; as run-mode (someip/glaze/foreman precedent).
(define deps '("base" "rackunit-lib"))

(define build-deps '("base" "scribble-lib" "racket-doc" "rackunit-doc" "rackunit-lib"))

(define scribblings '(("scribblings/dds.scrbl" ())))

(define pkg-desc
  "DDS/RTPS protocol stack: CDR (XCDR1) codec, RTPS 2.3 wire format, SPDP participant discovery, UDP multicast/unicast endpoints. Pure Racket, wire-compatible with the DDS ecosystem.")

(define pkg-authors '(turlamai))

(define license 'Apache-2.0)

;; `test/` runs under raco test; keep it out of setup compilation.
(define test-omit-paths '("test"))
