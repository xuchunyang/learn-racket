#lang racket

(define (ls file-or-dir)
  (if (file-exists? file-or-dir)
      (displayln file-or-dir)
      (for-each ls (directory-list file-or-dir #:build? #t))))

(module+ main
  (match (current-command-line-arguments)
    [(vector file-or-dir) (ls file-or-dir)]
    [_ (ls (current-directory))]))
