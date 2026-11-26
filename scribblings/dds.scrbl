#lang scribble/manual

@; SPDX-License-Identifier: Apache-2.0

@title{dds: a pure-Racket DDS/RTPS protocol stack}
@defmodule[dds]

This package implements the RTPS 2.3 wire protocol from the OMG
DDS-RTPS specification: CDR serialization, RTPS messages, and SPDP
participant discovery over UDP multicast. It is wire-compatible with
the DDS ecosystem (Cyclone DDS, eProsima Fast-DDS, ROS 2) and stays
pure Racket: no FFI, no external processes, @racket[base] alone.

The stack is built for tooling — monitors, dissectors, replay
harnesses and bridges — not for embedding in a hard-realtime DDS
deployment.

@table-of-contents[]

@; ---------------------------------------------------------------------
@section{Layers}

@itemlist[
 @item{@racket[(dds entities)] — GUIDs, entity ids, locators,
   well-known port formulas}
 @item{@racket[(dds cdr)] — CDR/XCDR1 value codec with the
   stream-relative alignment rule}
 @item{@racket[(dds message)] — RTPS header and submessages (DATA,
   HEARTBEAT, ACKNACK, GAP, INFO_TS, INFO_DST), parameter lists}
 @item{@racket[(dds transport)] — UDP endpoints with multicast join}
 @item{@racket[(dds spdp)] — participant discovery writer/reader with
   lease tracking}
 @item{@racket[(dds logging)] — pluggable logger topic @tt{dds}}]

@; ---------------------------------------------------------------------
@section{Discovering a domain}

@racketblock[
(require dds)

(define events '())
(define-values (reader handler)
  (make-spdp-reader
   #:event! (lambda (ev rec reason)
              (set! events (list* ev rec events)))))

(define ep
  (make-rtps-endpoint
   #:handler
   (lambda (header subs sender) (handler header subs sender))))

(define writer
  (make-spdp-writer
   #:send! (lambda (bs)
             (endpoint-send-multicast! ep "239.255.0.1" 7400 bs))))
]

After a few seconds @racket[reader]'s records contain every DDS
participant announcing on the segment (Cyclone DDS, Fast DDS, ROS 2
daemons and other @racket[dds] instances announce on the same
multicast group).

Roadmap: SEDP endpoint discovery and the reliable protocol subset
(P2), a QoS subset with transient-local (P3), and tooling — domain
monitor, CDR dissector, replay, SOME/IP-DDS bridge (P4).
