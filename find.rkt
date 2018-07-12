#lang racket/base

(define (find dir pattern)
  (for ([p (in-directory dir)])
    (when (regexp-match? pattern p)
      (printf "~a\n" p))))

(module+ main
  (require racket/match)
  (match (current-command-line-arguments)
    [(vector dir pattern) (find dir pattern)]
    [_ (find "." "\\.rkt$")]))
