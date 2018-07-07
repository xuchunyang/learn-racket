#lang racket/base

(require net/url)

(define (curl url)
  (define-values (status headers port)
    (http-sendrecv/url
     (string->url url)))

  (let rw ([b (read-byte port)])
    (unless (eof-object? b)
      (write-byte b)
      (rw (read-byte port)))))

;; $ racket curl.rkt
;; $ racket curl.rkt https://emacs-china.org
(module+ main
  (require racket/match)
  (require racket/string)
  (define url
    (match (current-command-line-arguments)
      [(vector) "http://example.com"]
      [(vector arg) (if (or (string-prefix? arg "http://")
                            (string-prefix? arg "https://"))
                        arg
                        (string-append "http://" arg))]))
  (curl url))
