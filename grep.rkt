#lang racket/base

;; XXX Add colors?

(define pattern (vector-ref (current-command-line-arguments) 0))

(define filename
  (if (> (vector-length (current-command-line-arguments)) 1)
      (vector-ref (current-command-line-arguments) 1)
      "-"))

(define input (if (equal? filename "-")
                  (current-input-port)
                  (open-input-file filename)))

(for ([line (in-lines input)])
  (when (regexp-match pattern line)
    (displayln line)))
