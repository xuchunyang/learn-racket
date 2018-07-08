#lang racket/base

(require json
         net/url
         racket/port
         racket/system)

(define (my-public-ip)
  (define-values (status headers port)
    (http-sendrecv/url
     ;; XXX https://api.ipify.org?format=json not work
     (string->url "https://api.ipify.org/?format=json")))
  (define ht (read-json port))
  (hash-ref ht 'ip))

;; $ ifconfig
;; $ ping -c 1 localhost
;; $ ip addr
(define (my-local-ip)
  (define output (with-output-to-string
                   (lambda () (system "ping -c 1 localhost"))))
  (list-ref (regexp-match #rx"\\((.+)\\)" output) 1))

(module+ main
  (printf "Public IP: ~a\n" (my-public-ip))
  (printf "Local  IP: ~a\n" (my-local-ip)))
