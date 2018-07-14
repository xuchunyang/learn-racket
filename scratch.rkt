#lang racket
(require web-server/servlet
         web-server/servlet-env)
 
(define (start request)
  (response/xexpr
   '(html
     (head (title "*scratch*")
           (link ((rel "stylesheet")
                  (href "/scratch.css")
                  (type "text/css"))))
     (body
      ;; XXX 怎么获得这里的数据, JavaScript ?
      (textarea ((rows "20") (cols "40")) "some text here")))))

(serve/servlet start
               #:extra-files-paths (list (current-directory)))
