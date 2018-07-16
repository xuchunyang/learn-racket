#lang racket/base

;; http://localhost:8000 => 0
;; http://localhost:8000/1/2/3 => 6

(require (prefix-in servlet: web-server/servlet-env)
         (prefix-in xexpr: web-server/http/xexpr)
         (prefix-in dispatch: web-server/dispatch)
         (prefix-in dispatch-log: web-server/dispatchers/dispatch-log))

(define (sum req is)
  (xexpr:response/xexpr
   `(html (body (h2 "URI 求和")
                (p ,(format "结果: ~a" (apply + is)))))))

(define (home req)
  (xexpr:response/xexpr
   '(html (body (h2 "URI 求和")))))

(define-values (sum-dispatch sum-url)
  (dispatch:dispatch-rules
   [((dispatch:integer-arg) ...) sum]
   [else home]))

(define (route-dispatch/log-middleware req)
  (display (dispatch-log:apache-default-format req))
  (flush-output)
  (sum-dispatch req))

(servlet:serve/servlet
 route-dispatch/log-middleware
 #:servlet-path "/"
 #:servlet-regexp #rx""
 #:stateless? #t)
