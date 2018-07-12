#lang racket/base

(define (ls file-or-dir)
  (if (file-exists? file-or-dir)
      (displayln file-or-dir)
      (for-each ls (directory-list file-or-dir #:build? #t))))

(define (ls2 dir)
  (for ([p (in-directory dir)])
    (printf "~a\n" p)))

(module+ main
  (require racket/match)
  (match (current-command-line-arguments)
    [(vector file-or-dir) (ls2 file-or-dir)]
    [_ (ls2 (current-directory))]))
