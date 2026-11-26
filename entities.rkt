#lang racket/base

;; RTPS entity identities and network locators (DDS-RTPS 2.3, chapter
;; 8.2 "Description of the conceptual entities" and 8.4 "Locators").
;;
;; A GUID is 12 octets of participant prefix + 4 octets of entity id;
;; the entity id's last octet is the kind (with key / without key /
;; built-in). Locators are 24 octets on the wire: kind, port, address
;; (16 octets; UDPv4 addresses are zero-padded in front).
;;
;; Wire rules implemented here:
;;   [8.2.4]  GUID layout: prefix (12) + entity id (4)
;;   [8.2.4]  entity id kind octets, table: with-key 0x02/0x07,
;;            no-key 0x03/0x04, built-in participant 0x01
;;   [8.4.1]  locator layout: kind (4) + port (4) + address (16);
;;            LOCATOR_KIND_UDPV4 = 1, IPv4 lives in the last 4 octets
;;   [9.6.3]  well-known port mapping:
;;            metatraffic multicast = PB + DG*d + d0
;;            user multicast        = PB + DG*d + d1
;;            metatraffic unicast   = PB + DG*d + PG*p + d2
;;            user unicast          = PB + DG*d + PG*p + d3
;;            with PB=7400, DG=250, PG=2, d0=0, d1=10, d2=1, d3=11

(require racket/contract
         racket/match
         racket/random
         racket/string
         "logging.rkt"
         "private/bytes.rkt")

(provide (struct-out locator)
         udpv4-locator
         locator-udpv4?
         locator-ip
         locator->bytes
         bytes->locator
         locator=?

         (struct-out guid)
         make-guid
         guid->bytes
         bytes->guid
         guid=?
         guid-prefix=?
         random-guid-prefix
         entity-id-spdp-participant
         entity-id-sedp-publications-writer
         entity-id-sedp-publications-reader
         entity-id-sedp-subscriptions-writer
         entity-id-sedp-subscriptions-reader
         entity-id-participant-message-writer
         entity-id-participant-message-reader
         entity-id-unknown
         entity-id-key
         entity-id-kind

         default-port-base default-domain-gain default-participant-gain
         d0-multicast-discovery d1-multicast-user d2-unicast-discovery d3-unicast-user
         discovery-multicast-port
         user-multicast-port
         discovery-unicast-port
         user-unicast-port

         default-discovery-multicast-address
         default-builtin-endpoints-set
         builtin-endpoint-set?
         check-prefix)

(module+ test (require rackunit))

;; ------------------------------------------------------------------
;; Locators

(struct locator (kind port address) #:transparent)

(define LOCATOR-KIND-UDPV4 1)

;; Build a UDPv4 locator from a host string. The 16-octet address field
;; is zero-padded in front of the 4 address octets [8.4.1].
(define (udpv4-locator host port)
  (define parts
    (match (string-split host ".")
      [(list a b c d) (map (lambda (s) (string->number s 10)) (list a b c d))]
      [_ (raise-argument-error 'udpv4-locator "IPv4 dotted quad" host)]))
  (unless (andmap (lambda (n) (and n (<= 0 n 255))) parts)
    (raise-argument-error 'udpv4-locator "IPv4 dotted quad" host))
  (unless (and (exact-positive-integer? port) (< port 65536))
    (raise-argument-error 'udpv4-locator "port" port))
  (locator LOCATOR-KIND-UDPV4 port
           (bytes->bytes16 (list->bytes parts))))

(define (bytes->bytes16 ipv4)
  (bytes-append (make-bytes 12 0) ipv4 (make-bytes 0)))

(define (locator-udpv4? loc)
  (= (locator-kind loc) LOCATOR-KIND-UDPV4))

;; Dotted quad of a UDPv4 locator.
(define (locator-ip loc)
  (unless (locator-udpv4? loc)
    (raise-argument-error 'locator-ip "udpv4 locator" loc))
  (define a (locator-address loc))
  (format "~a.~a.~a.~a"
          (bytes-ref a 12) (bytes-ref a 13) (bytes-ref a 14) (bytes-ref a 15)))

;; 24-octet wire form: kind (4) + port (4) + address (16).
(define (locator->bytes loc)
  (define c (make-cursor #:le? #t))
  (cur-i32 c (locator-kind loc))
  (cur-u32 c (locator-port loc))
  (cur-raw c (locator-address loc))
  (cursor->bytes c))

(define (bytes->locator bs [off 0])
  (unless (>= (bytes-length bs) (+ off 24))
    (raise-argument-error 'bytes->locator "24-octet locator" bs))
  (define r (make-reader bs off (+ off 24) #t))
  (define kind (rd-i32 r))
  (define port (rd-u32 r))
  (locator kind port (rd-raw r 16)))

(define (locator=? a b)
  (and (= (locator-kind a) (locator-kind b))
       (= (locator-port a) (locator-port b))
       (equal? (locator-address a) (locator-address b))))

;; ------------------------------------------------------------------
;; GUIDs

(struct guid (prefix entity) #:transparent)
;; prefix: 12-octet byte string, entity: 4-octet byte string

(define (check-prefix who bs)
  (unless (and (bytes? bs) (= (bytes-length bs) 12))
    (raise-argument-error who "12-octet guid prefix" bs)))

(define (check-entity who bs)
  (unless (and (bytes? bs) (= (bytes-length bs) 4))
    (raise-argument-error who "4-octet entity id" bs)))

(define (make-guid prefix entity)
  (check-prefix 'make-guid prefix)
  (check-entity 'make-guid entity)
  (guid prefix entity))

(define (guid->bytes g) (bytes-append (guid-prefix g) (guid-entity g)))

(define (bytes->guid bs [off 0])
  (unless (>= (bytes-length bs) (+ off 16))
    (raise-argument-error 'bytes->guid "16-octet guid" bs))
  (guid (subbytes bs off (+ off 12)) (subbytes bs (+ off 12) (+ off 16))))

(define (guid=? a b) (and (guid? a) (guid? b)
                          (guid-prefix=? a b)
                          (bytes=? (guid-entity a) (guid-entity b))))

(define (guid-prefix=? a b) (bytes=? (guid-prefix a) (guid-prefix b)))

;; Generate a participant guid prefix: per [8.2.4] the first two bits
;; of the first octet signal a uniquely generated prefix; the rest
;; comes from the OS RNG (vendors use vendor id / pid / timestamp in
;; the remaining octets, which we mirror loosely).
(define (random-guid-prefix [vendor-id (bytes 1 240)])
  (define rnd (crypto-random-bytes 10))
  (bytes-append
   vendor-id
   ;; the two high bits are 0b01, the low bits come from the RNG
   (bytes (bitwise-ior #b01000000 (bitwise-and (bytes-ref rnd 0) #b00111111)))
   (subbytes rnd 1)))

;; ------------------------------------------------------------------
;; Entity ids (3 key octets + kind octet)

;; built-in SPDP participant reader/writer (both share the id)
(define entity-id-spdp-participant (bytes #x00 #x01 #x00 #x01))
;; built-in SEDP publications (announced writers) reader/writer
(define entity-id-sedp-publications-writer (bytes #x00 #x03 #x00 #x02))
(define entity-id-sedp-publications-reader (bytes #x00 #x03 #x00 #x07))
;; built-in SEDP subscriptions (announced readers) reader/writer
(define entity-id-sedp-subscriptions-writer (bytes #x00 #x04 #x00 #x02))
(define entity-id-sedp-subscriptions-reader (bytes #x00 #x04 #x00 #x07))
;; built-in participant message (liveness) reader/writer
(define entity-id-participant-message-writer (bytes #x00 #x02 #x00 #x02))
(define entity-id-participant-message-reader (bytes #x00 #x02 #x00 #x07))
;; "every entity" placeholder used in DATA submessages
(define entity-id-unknown (bytes 0 0 0 0))

(define (entity-id-key eid) (subbytes eid 0 3))
(define (entity-id-kind eid) (bytes-ref eid 3))

;; ------------------------------------------------------------------
;; Well-known ports [9.6.3]

(define default-port-base 7400)        ; PB
(define default-domain-gain 250)       ; DG
(define default-participant-gain 2)    ; PG
(define d0-multicast-discovery 0)
(define d1-multicast-user 10)
(define d2-unicast-discovery 1)
(define d3-unicast-user 11)

(define (discovery-multicast-port [domain-id 0]
                                  [pb default-port-base]
                                  [dg default-domain-gain])
  (+ pb (* dg domain-id) d0-multicast-discovery))

(define (user-multicast-port [domain-id 0]
                             [pb default-port-base]
                             [dg default-domain-gain])
  (+ pb (* dg domain-id) d1-multicast-user))

(define (discovery-unicast-port [participant-id 0] [domain-id 0]
                                [pb default-port-base]
                                [dg default-domain-gain]
                                [pg default-participant-gain])
  (+ pb (* dg domain-id) (* pg participant-id) d2-unicast-discovery))

(define (user-unicast-port [participant-id 0] [domain-id 0]
                           [pb default-port-base]
                           [dg default-domain-gain]
                           [pg default-participant-gain])
  (+ pb (* dg domain-id) (* pg participant-id) d3-unicast-user))

(define default-discovery-multicast-address "239.255.0.1")

;; ------------------------------------------------------------------
;; Builtin endpoint set bits [7.3.1.2.4, table]

(define (builtin-endpoint-set? v) (and (exact-nonnegative-integer? v) (< v (expt 2 64))))

(define builtin-endpoint-participant-announcer  (arithmetic-shift 1 0))
(define builtin-endpoint-participant-detector   (arithmetic-shift 1 1))
(define builtin-endpoint-publications-writer    (arithmetic-shift 1 2))
(define builtin-endpoint-publications-detector  (arithmetic-shift 1 3))
(define builtin-endpoint-subscriptions-writer   (arithmetic-shift 1 4))
(define builtin-endpoint-subscriptions-detector (arithmetic-shift 1 5))
(define builtin-endpoint-participant-message-writer (arithmetic-shift 1 10))
(define builtin-endpoint-participant-message-reader (arithmetic-shift 1 11))

;; What this package speaks today: SPDP now, SEDP and participant
;; messages on the roadmap; announce what is actually implemented so
;; peers do not open endpoints that never answer.
(define default-builtin-endpoints-set
  (bitwise-ior builtin-endpoint-participant-announcer
               builtin-endpoint-participant-detector))

;; ------------------------------------------------------------------
;; Tests

(module+ test
  (define loc (udpv4-locator "239.255.0.1" 7400))
  (check-equal? (locator-ip loc) "239.255.0.1")
  (check-equal? (locator->bytes loc)
                (bytes 1 0 0 0        ; kind 1, little endian
                       #xE8 #x1C 0 0  ; port 7400 = 0x1CE8, little endian
                       0 0 0 0 0 0 0 0 0 0 0 0 ; zero padding
                       #xEF #xFF 0 1)          ; 239.255.0.1
                "locator wire form: kind, port, padded address")
  (check-true (locator=? loc (bytes->locator (locator->bytes loc))))

  (check-exn exn:fail? (lambda () (udpv4-locator "999.1.1.1" 7400)))
  (check-exn exn:fail? (lambda () (udpv4-locator "example.invalid" 7400)))

  (define prefix (random-guid-prefix (bytes 1 240)))
  (check-equal? (bytes-length prefix) 12)
  (check-equal? (bitwise-and (bytes-ref prefix 2) #xC0) #x40
                "first octet flags a uniquely generated prefix")
  (define g (make-guid prefix entity-id-spdp-participant))
  (check-equal? (bytes-length (guid->bytes g)) 16)
  (check-true (guid=? g (bytes->guid (guid->bytes g))))
  (check-equal? (entity-id-key entity-id-sedp-publications-writer)
                (bytes 0 3 0))
  (check-equal? (entity-id-kind entity-id-sedp-publications-reader) #x07)

  (check-equal? (discovery-multicast-port 0) 7400)
  (check-equal? (user-multicast-port 0) 7410)
  (check-equal? (discovery-unicast-port 0 0) 7401)
  (check-equal? (discovery-unicast-port 1 0) 7403)
  (check-equal? (user-unicast-port 0 0) 7411)
  (check-equal? (discovery-multicast-port 1) 7650)
  (check-equal? (discovery-unicast-port 2 1) 7655)

  (check-true (builtin-endpoint-set? default-builtin-endpoints-set)))
