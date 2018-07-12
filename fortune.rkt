#lang racket
(require web-server/servlet
         web-server/servlet-env)

(define (shell-command-to-string command)
  (with-output-to-string (lambda () (system command))))

(define (start req)
  (response/xexpr
   `(html (head (title "Fortune"))
          (body (pre
                 ,(with-output-to-string (lambda () (system "fortune"))))))))

(match (system-type)
  ['macosx (serve/servlet start
                          #:servlet-regexp #rx""
                          #:servlet-path "/")]
  ;; Ubuntu 服务器
  ['unix (serve/servlet start
                        #:servlet-regexp #rx""
                        #:servlet-path "/"
                        #:launch-browser? #f
                        #:port 80
                        #:listen-ip #f)])
