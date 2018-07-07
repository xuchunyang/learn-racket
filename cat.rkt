#lang racket/base

(let rw ([b (read-byte)])
  (unless (eof-object? b)
    (write-byte b)
    (rw (read-byte))))

;; raco test cat.rkt
(module test racket/base
  (require rackunit
           racket/port
           racket/system)
  (check-equal? (with-output-to-string (lambda () (system "cal")))
                (with-output-to-string (lambda () (system "cal | racket cat.rkt")))))
