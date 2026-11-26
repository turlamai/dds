# dds

A pure-Racket DDS/RTPS protocol stack: CDR (XCDR1) serialization, RTPS
2.3 wire format, SPDP participant discovery, UDP multicast/unicast
endpoints. Wire-compatible with the DDS ecosystem — Cyclone DDS,
eProsima Fast-DDS, ROS 2 — with zero FFI: the shipped collection
depends on `base` alone.

The sibling [`someip`](https://github.com/turlamai/someip) package
implements SOME/IP; together they cover the two Adaptive AUTOSAR
communication bindings for Racket-based tooling.

## Status / roadmap

| Phase | Scope | State |
|---|---|---|
| P0 | CDR/XCDR1 codec, encapsulations, alignment | done |
| P1 | RTPS messages, UDP transport, SPDP discovery | done |
| P2 | SEDP endpoint discovery, reliable protocol subset (heartbeat/acknack/gap) | done |
| P3 | QoS subset (reliable, transient-local, keep-last), programmatic topic types | done |
| P4 | Tooling: domain monitor, CDR dissector, replay | planned |

The SOME/IP-DDS bridge and the CAN signal side land as separate
packages: [`someip`](https://github.com/turlamai/someip) + `dds` stay
protocol-pure, a `dbc` package adds DBC/CAN signal codecs, and `s2s`
is the mapping engine with can/someip/dds bindings (the user-space
take on RTA-VRTE's Signal-to-Service conversion). SEDP (P2) comes
first — DDS delivery requires endpoint discovery.

Reliable delivery, SEDP endpoint discovery, a topic type DSL and a
minimal participant/topic pub/sub API (reliable, transient-local,
keep-last) are in; tooling comes with P4.

## Example

```racket
(require dds)

(define speed-type
  (struct-t (struct-member 'kph u16-t)
            (struct-member 'label string-t)))

;; one participant: endpoint + SPDP + SEDP wired together
(define p (make-participant))

(define t (participant-topic! p "Speed" speed-type
            #:reliability 'reliable
            #:durability 'transient-local
            #:history-depth 5))

;; publish
(define w (topic-writer! t))
(writer-write! w '((kph . 42) (label . "front-left")))

;; subscribe (including late joiners: they receive the retained
;; transient-local history)
(topic-reader! t (lambda (value sn) (printf "~a\n" value)))
```

Lower layers stay exposed: `make-spdp-reader`/`make-rtps-endpoint`
for domain monitoring, `(dds message)` for raw RTPS, `(dds cdr)` for
CDR payloads. Any Cyclone DDS / Fast DDS / ROS 2 participant on the
segment appears in SPDP discovery (and vice versa).

## Design notes

- **Alignment.** CDR alignment is counted from the start of the
  serialized stream *including* the 4-octet encapsulation header —
  the classic CDR pitfall, encoded once in `(dds cdr)`.
- **Endianness.** RTPS flips endianness per submessage (flag bit 0)
  and CDR per encapsulation scheme; the byte layer carries the order
  in the stream object. This stack emits little-endian by default
  (the ecosystem norm) and decodes both.
- **SPDP.** Cyclone-style startup burst, steady announcements every
  5 s, disposal via `STATUS_INFO`, lease-duration expiry watch.
- **Ports.** Well-known port mapping (PB=7400, DG=250, PG=2) with the
  standard `d0..d3` offsets, see `(dds entities)`.
- **Vendor id.** `0x01F0` (unassigned tail of the OMG 0x01xx block).
- No `SO_REUSEADDR` in Racket UDP: one endpoint per port per process;
  the P2 participant layout splits metatraffic/user unicast sockets.

## Development

```sh
raco test .        # unit tests, inline per module
raco setup dds     # build the scribble docs
```

The automated suite avoids multicast; a loopback smoke test lives in
`test/interop.rkt` and a real Cyclone DDS interop check is documented
there as well.

## License

Apache-2.0
