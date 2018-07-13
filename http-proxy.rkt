#lang racket/base

(require net/http-client
         net/url
         racket/list)

(define (ensure-non-empty s)
  (if (string=? "" s)
      "/"
      s))

(define proxy-server (make-parameter "127.0.0.1"))
(define proxy-port (make-parameter 1087))

(define (http-proxy-sendrecv/url u
                                 #:method [method-bss #"GET"]
                                 #:headers [headers-bs empty]
                                 #:data [data #f]
                                 #:content-decode [decodes '(gzip)])
  (define ssl?
    (equal? (url-scheme u) "https"))
  (define port
    (or (url-port u)
        (if ssl?
            443
            80)))
  (define hc (http-conn))
  (http-conn-open!
   hc
   (url-host u)
   #:ssl? (call-with-values
           (lambda ()
             (http-conn-CONNECT-tunnel (proxy-server)
                                       (proxy-port)
                                       (url-host u)
                                       port
                                       #:ssl? 'auto))
           list)
   #:port port)
  (http-conn-sendrecv! hc
                       (ensure-non-empty
                        (url->string
                         (make-url #f #f #f #f
                                   (url-path-absolute? u)
                                   (url-path u)
                                   (url-query u)
                                   (url-fragment u))))))

(provide http-proxy-sendrecv/url)
