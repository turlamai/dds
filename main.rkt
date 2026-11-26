#lang racket/base

;; `dds` facade: the whole stack from one require.
;;
;; Layers, bottom to top:
;;   (dds entities)   GUIDs, entity ids, locators, well-known ports
;;   (dds cdr)        CDR/XCDR1 value codec + encapsulation
;;   (dds message)    RTPS 2.3 header/submessages, parameters, DATA,
;;                    HEARTBEAT, ACKNACK, INFO_TS, INFO_DST
;;   (dds transport)  UDP endpoints with multicast join
;;   (dds spdp)       participant discovery writer/reader
;;   (dds logging)    pluggable logger topic

(require "cdr.rkt"
         "entities.rkt"
         "logging.rkt"
         "message.rkt"
         "spdp.rkt"
         "transport.rkt")

(provide (all-from-out "cdr.rkt")
         (all-from-out "entities.rkt")
         (all-from-out "logging.rkt")
         (all-from-out "message.rkt")
         (all-from-out "spdp.rkt")
         (all-from-out "transport.rkt"))
