#lang racket/base

;; UDP transport for RTPS (DDS-RTPS 2.3, chapter 8.4 "Transport
;; mapping" and 9.6.3 well-known ports).
;;
;; Wire rules implemented here:
;;   [9.6.3] metatraffic multicasts go to the well-known address and
;;           port (239.255.0.1:7400 for domain 0); the endpoint binds
;;           the multicast port and joins the group so unicast and
;;           multicast RTPS datagrams arrive on one socket
;;           (the same layout the someip package uses for SD)
;;   [8.4.2] datagrams carry complete RTPS messages; each is decoded
;;           into header + submessages and dispatched to the handler
;;
;; One endpoint owns one custodian; closing it tears down the socket
;; and reader thread. This Racket version has no SO_REUSEADDR for UDP
;; sockets, so a second endpoint cannot bind the same port in this
;; process - a full participant needs the multi-socket layout planned
;; for the P2 reliability layer.

(require racket/contract
         racket/format
         racket/match
         racket/udp
         "entities.rkt"
         "message.rkt"
         "logging.rkt")

(provide (struct-out sender)
         (struct-out rtps-endpoint)
         make-rtps-endpoint
         endpoint-closed?
         endpoint-send!
         endpoint-send-multicast!
         endpoint-close!
         endpoint-udp-port)

(module+ test (require rackunit racket/list))

;; Sender identity of a received datagram.
(struct sender (host port) #:transparent)

(struct rtps-endpoint
  (custodian
   sock
   port
   handler        ; (-> rtps-header? (listof submessage?) sender? any/c)
   multicast-groups
   closed?)
  #:mutable)

;; Build an endpoint bound to `bind-port` (default: the well-known
;; discovery multicast port) and joined to `multicast-groups`.
;; `handler` receives every decoded RTPS message with its sender.
(define (make-rtps-endpoint
         #:bind-port [bind-port (discovery-multicast-port 0)]
         #:multicast-groups [multicast-groups
                             (list default-discovery-multicast-address)]
         #:loopback? [loopback? #t]
         #:handler handler)
  (unless (procedure? handler)
    (raise-argument-error 'make-rtps-endpoint "procedure?" handler))
  ;; port 0 picks an ephemeral port
  (unless (and (exact-nonnegative-integer? bind-port) (< bind-port 65536))
    (raise-argument-error 'make-rtps-endpoint "port" bind-port))
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define sock (udp-open-socket #f))
    (udp-bind! sock #f bind-port)
    (define-values (_lh bound-port _rh _rp)
      (udp-addresses sock #t))
    (for ([group-address multicast-groups])
      (udp-multicast-join-group! sock group-address #f)
      (udp-multicast-set-loopback! sock loopback?))
    (define ep-placeholder
      (rtps-endpoint cust sock bound-port handler multicast-groups #f))
    ;; reader thread: decode and dispatch; a bad datagram logs a
    ;; warning and never kills the thread
    (define buffer (make-bytes 65536))
    (thread
     (lambda ()
       (let loop ()
         (define-values (n host port)
           (with-handlers ([exn:fail? (lambda (_) (values eof #f #f))])
             (udp-receive! sock buffer)))
         (unless (eof-object? n)
           (when (positive? n)
             (with-handlers ([exn:fail? (lambda (e)
                                          (dds-log-warning
                                           "transport: dropped datagram from ~a:~a: ~a"
                                           host port (exn-message e)))])
               (define-values (header subs)
                 (decode-rtps-message (subbytes buffer 0 n)))
               ((rtps-endpoint-handler ep-placeholder)
                header subs (sender host port))))
           (loop)))))
    ep-placeholder))

;; Send a raw datagram (an encoded RTPS message) to a unicast address.
(define (endpoint-send! ep host port bs)
  (when (endpoint-closed? ep)
    (raise-argument-error 'endpoint-send! "open endpoint" ep))
  (udp-send-to (rtps-endpoint-sock ep) host port bs))

;; Send to a multicast group; same socket, multicast TTL is left at
;; the system default so announcements stay in the local segment.
(define (endpoint-send-multicast! ep group port bs)
  (endpoint-send! ep group port bs))

(define (endpoint-closed? ep) (rtps-endpoint-closed? ep))

(define (endpoint-close! ep)
  (unless (rtps-endpoint-closed? ep)
    (set-rtps-endpoint-closed?! ep #t)
    (custodian-shutdown-all (rtps-endpoint-custodian ep))))

(define (endpoint-udp-port ep) (rtps-endpoint-port ep))

;; ------------------------------------------------------------------
;; Tests
;;
;; Binding sockets in tests is environment sensitive, so the automated
;; suite covers construction/validation only; a loopback exchange runs
;; in test/interop.rkt (see the package README).

(module+ test
  (define ep (make-rtps-endpoint #:bind-port 0
                                 #:multicast-groups '()
                                 #:handler void))
  (check-true (exact-positive-integer? (endpoint-udp-port ep)))
  (check-false (endpoint-closed? ep))
  (endpoint-close! ep)
  (check-true (endpoint-closed? ep))
  (check-exn exn:fail?
             (lambda () (endpoint-send! ep "127.0.0.1" 7400 (make-bytes 4)))))
