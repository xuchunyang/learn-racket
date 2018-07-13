#lang racket
(require html-parsing
         racket/random
         sxml
         web-server/servlet
         web-server/servlet-env
         "http-proxy.rkt")

(define (extract-image-urls url)
  (define-values (status headers html) (http-proxy-sendrecv/url (string->url url)))
  (define xexp (html->xexp html))
  (map cadr ((sxpath "//img/@src") xexp)))

(define image-urls (extract-image-urls
                    "https://onejav.com/"
                    #;"https://en.wikipedia.org/wiki/Main_Page"))

(define (start req)
  (response/xexpr
   `(html (head (title "Random Image"))
          (body (img ([src ,(random-ref image-urls)]))))))

(match (system-type)
  ['macosx (serve/servlet start
                          #:servlet-regexp #rx""
                          #:servlet-path "/"
                          #:ssl? #t
                          #:ssl-cert (expand-user-path "~/example.com+5.pem")
                          #:ssl-key (expand-user-path "~/example.com+5-key.pem"))]
  ;; Ubuntu 服务器
  ['unix (serve/servlet start
                        #:servlet-regexp #rx""
                        #:servlet-path "/"
                        #:launch-browser? #f
                        #:port 80
                        #:listen-ip #f)])
