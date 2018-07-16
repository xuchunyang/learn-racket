#lang racket/base

;; http://localhost:8000 => 0
;; http://localhost:8000/1/2/3 => 6

(require (prefix-in servlet: web-server/servlet-env)
         (prefix-in xexpr: web-server/http/xexpr)
         (prefix-in dispatch: web-server/dispatch)
         (prefix-in dispatch-log: web-server/dispatchers/dispatch-log)
         (prefix-in request-structs: web-server/http/request-structs)
         (prefix-in response-structs: web-server/http/response-structs))

(define (sum req is)
  (define heads (request-structs:request-headers/raw req))
  ;; #"curl/7.54.0"
  (define user-agent (request-structs:header-value
                      (request-structs:headers-assq* #"user-agent" heads)))
  (define result (apply + is))
  (if (regexp-match #rx"^curl" user-agent)
      (response-structs:response
       200
       #"OK"
       (current-seconds)
       #"text/plain; charset=utf-8"
       (list)
       (λ (op) (display result op)))
      (xexpr:response/xexpr
       `(html (body (h2 "URI 求和")
                    (p ,(format "结果: ~a" result)))))))

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
