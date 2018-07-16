;; Source: http://notes.eatonphil.com/walking-through-a-basic-racket-web-service.html
#lang racket/base

(require (prefix-in dispatch: web-server/dispatch)
         (prefix-in dispatch-log: web-server/dispatchers/dispatch-log)
         (prefix-in xexpr: web-server/http/xexpr)
         (prefix-in servlet: web-server/servlet-env))

(define (not-found-route request)
  (xexpr:response/xexpr
   `(html (body (h2 "Uh-oh! Page not found.")))))

(define (home-route request)
  (xexpr:response/xexpr
   `(html (body (h2 "Look ma, no state!!!!!!!!!")))))

(define-values (route-dispatch route-url)
  (dispatch:dispatch-rules
   [("") home-route]
   [else not-found-route]))

(define (route-dispatch/log-middleware req)
  (display (dispatch-log:apache-default-format req))
  (flush-output)
  (route-dispatch req))

(servlet:serve/servlet
 route-dispatch/log-middleware
 #:servlet-path "/"
 #:servlet-regexp #rx""
 #:stateless? #t)
