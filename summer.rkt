#lang racket
(require web-server/servlet
         web-server/servlet-env)
 
(define (start request)
  (render-page request ""))

(define (summer bindings)
  (let ([a (string->number (extract-binding/single 'a bindings))]
        [b (string->number (extract-binding/single 'b bindings))])
    `(p ,(format "~a + ~a = ~a" a b (+ a b)))))

(define (render-page request result)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "求和"))
       (body (h1 "求和")
             (form
              ((action
                ,(embed/url submit-handler)))
              (input ((name "a") (placeholder "数字")))
              (input ((name "b") (placeholder "数字")))
              (input ((type "submit"))))
             ,result))))
  (define (submit-handler request)
    (render-page (redirect/get) (summer (request-bindings request))))
  (send/suspend/dispatch response-generator))

(serve/servlet start #:listen-ip #f)
