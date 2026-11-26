#lang racket/base

;; `dds` facade: the whole stack from one require.
;;
;; Layers, bottom to top:
;;   (dds entities)   GUIDs, entity ids, locators, well-known ports
;;   (dds cdr)        CDR/XCDR1 value codec + encapsulation
;;   (dds message)    RTPS 2.3 header/submessages, parameters, DATA,
;;                    HEARTBEAT, ACKNACK, GAP, INFO_TS, INFO_DST
;;   (dds reliable)   reliable writer/reader state machines
;;   (dds transport)  UDP endpoints with multicast join
;;   (dds spdp)       participant discovery writer/reader
;;   (dds sedp)       endpoint discovery over the reliable machines
;;   (dds topic-type) programmatic topic type DSL (CDR codecs)
;;   (dds topic)      participant/topic pub/sub API
;;   (dds logging)    pluggable logger topic

(require "cdr.rkt"
         "entities.rkt"
         "logging.rkt"
         "message.rkt"
         "reliable.rkt"
         "sedp.rkt"
         "spdp.rkt"
         "topic-type.rkt"
         "topic.rkt"
         "transport.rkt")

(provide (all-from-out "cdr.rkt")
         (all-from-out "entities.rkt")
         (all-from-out "logging.rkt")
         (all-from-out "message.rkt")
         (all-from-out "reliable.rkt")
         (all-from-out "sedp.rkt")
         (all-from-out "spdp.rkt")
         (all-from-out "topic-type.rkt")
         (all-from-out "topic.rkt")
         (all-from-out "transport.rkt"))
