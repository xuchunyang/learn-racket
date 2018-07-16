#lang racket

(require xml net/url)

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (define t (thread loop))
    (lambda ()
      (custodian-shutdown-all main-cust))))

(define (accept-and-handle listener)
  ;; 托管人
  (define cust (make-custodian))
  ;; 50 MB
  (custodian-limit-memory cust (* 50 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (define t (thread
               (λ ()
                 (handle in out)
                 (close-input-port in)
                 (close-output-port out))))
    (thread (lambda ()
              (sleep 10)
              (custodian-shutdown-all cust)))))

(define (handle in out)
  (define req
    ;; 匹配第一行以取出 Request
    (regexp-match #rx"GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ;; 跳过剩余的 Header
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; 分发
    (let ([xexpr (dispatch (list-ref req 1))])
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  (define url (string->url str-path))
  (define path (map path/param-path (url-path url)))
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      (h (url-query url))
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unkown page: "
                    ,str-path)))))

(define dispatch-table (make-hash))
(hash-set! dispatch-table "hello"
           (λ (query)
             `(html (body "Hello, World!"))))

(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"]
                                      [value ""]))
                (input ([type "hidden"] [name "hidden"]
                                        [value ,hidden]))
                (input ([type "submit"] [name "enter"]
                                        [value "Enter"]))))))

(define (many query)
  (build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)
