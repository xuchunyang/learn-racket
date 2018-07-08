#lang racket/base

(define (wc)
  (for/fold ([chars 0]
             [lines 0]
             [iswspace #t]
             [words 0]
             #:result (values chars lines words))
            ([char (in-port read-char)])
    (values (add1 chars)
            (if (eqv? char #\newline)
                (add1 lines)
                lines)
            (char-whitespace? char)
            (if (and iswspace (not (char-whitespace? char)))
                (add1 words)
                words))))

(module+ test
  (require rackunit)
  (require racket/port)
  (let-values ([(chars lines words)
                (with-input-from-string "Hello, world!\n"
                  (lambda () (wc)))])
    (check-eqv? chars 14)
    (check-eqv? lines 1)
    (check-eqv? words 2)))

(module+ main
  (let-values ([(chars lines words) (wc)])
    (printf "~a ~a ~a" lines words chars)))
