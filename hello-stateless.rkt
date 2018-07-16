#lang racket/base

(require web-server/http
         web-server/stuffers)

(provide interface-version stuffer start)

(define interface-version 'stateless)

(define stuffer
  (stuffer-chain
   serialize-stuffer
   (md5-stuffer (build-path (find-system-path 'home-dir) ".urls"))))

(define (start req)
  (response/xexpr
   `(html (body (h2 "Look ma, no state!")))))

(module+ main
  (require web-server/servlet-env)
  (serve/servlet
   start
   #:servlet-path "/"
   #:servlet-regexp #rx""
   #:stateless? #t))
