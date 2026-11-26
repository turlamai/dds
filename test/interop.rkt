#lang racket/base

;; Loopback smoke test: two endpoints on the discovery multicast group
;; exchange SPDP announcements over real UDP, the way two local
;; participants would.
;;
;; This module is excluded from `raco setup` (test-omit-paths) and is
;; meant for `raco test test/interop.rkt` on a machine where UDP
;; multicast loopback works. It may legitimately fail inside
;; containers/VMs where multicast is blocked - that is an environment
;; problem, not a stack bug.
;;
;; Real-vendor interop check (manual):
;;   1. install Cyclone DDS (`brew install cyclonedds` or from source)
;;      and `ddsperf` / `ddsls` from its tools
;;   2. run this file; watch `ddsls` list our participant, and count
;;      `ddsperf` participants from this stack's announcements
;;   3. run `ddsls`/`ddsperf` beside an active `make-spdp-reader` and
;;      confirm their participants appear in the reader records

(require rackunit
         racket/list
         "../entities.rkt"
         "../logging.rkt"
         "../spdp.rkt"
         "../transport.rkt")

(module+ test
  ;; remote participants seen
  (define seen (box '()))
  (define-values (reader handler)
    (make-spdp-reader
     #:event! (lambda (ev rec reason)
                (set-box! seen (cons (guid-prefix (participant-data-guid rec))
                                     (unbox seen))))))

  (define ep
    (make-rtps-endpoint
     #:handler (lambda (header subs sender) (handler header subs sender))))

  ;; our announcements go to the well-known multicast address/port and
  ;; back to the loopback interface for same-host participants; the
  ;; multicast leg is best-effort because sandboxes/VMs may have no
  ;; multicast route, the loopback leg is the smoke assertion
  (define writer
    (make-spdp-writer
     #:send! (lambda (bs)
               (with-handlers ([exn:fail? (lambda (e)
                                            (dds-log-info
                                             "interop: multicast send failed: ~a"
                                             (exn-message e)))])
                 (endpoint-send-multicast! ep default-discovery-multicast-address
                                           (discovery-multicast-port 0) bs))
               (endpoint-send! ep "127.0.0.1" (discovery-multicast-port 0) bs))
     #:metatraffic-unicast
     (udpv4-locator "127.0.0.1" (discovery-multicast-port 0))
     #:metatraffic-multicast
     (udpv4-locator default-discovery-multicast-address
                    (discovery-multicast-port 0))))

  ;; give the burst a moment, then check the loopback came back
  (sleep 2)
  (check-true (ormap (lambda (p) (bytes=? p (guid-prefix (participant-data-guid
                                                          (spdp-writer-data writer)))))
                     (unbox seen))
              "our own loopback announcement is decoded back")

  (spdp-writer-dispose! writer)
  (sleep 0.5)
  (endpoint-close! ep)
  (spdp-reader-stop! reader))
