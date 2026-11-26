#lang racket/base

;; Pluggable logging for the dds package.
;;
;; All modules log through the "dds" logger topic, so applications can
;; route, filter or silence the stack with Racket's standard logger
;; facilities. Library code must never print directly.

(require racket/contract
         racket/format)

(provide current-dds-logger
         dds-log-debug
         dds-log-info
         dds-log-warning
         dds-log-error)

(define dds-logger
  (make-logger 'dds #f))

;; Parameter controlling which logger receives dds diagnostics.
;; Applications may replace it (e.g. with their application logger) to
;; aggregate stack logs.
(define current-dds-logger
  (make-parameter dds-logger (lambda (v) (or (logger? v) (eq? v #f)))))

(define (dds-log level fmt . args)
  (define lg (current-dds-logger))
  (when lg
    (log-message lg level
                 (string->symbol (format "~a" (logger-name lg)))
                 (apply format fmt args) #f)))

(define (dds-log-debug fmt . args) (apply dds-log 'debug fmt args))
(define (dds-log-info fmt . args) (apply dds-log 'info fmt args))
(define (dds-log-warning fmt . args) (apply dds-log 'warning fmt args))
(define (dds-log-error fmt . args) (apply dds-log 'error fmt args))
