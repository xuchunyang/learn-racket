#lang racket
(require web-server/servlet
         web-server/servlet-env)

(define contents "some text here")

(define (render-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "*scratch*")
             (link ((rel "stylesheet")
                    (href "/scratch.css")
                    (type "text/css"))))
       (body
        (form ((action ,(embed/url save-textarea-handler)))
              (textarea ((name "textarea")
                         (rows "20")
                         (cols "40"))
                        ,contents)
              (input ((type "submit")
                      (value "Save"))))))))
  (define (save-textarea-handler request)
    (set! contents
          (string-append (extract-binding/single 'textarea (request-bindings request))
                         "\n"
                         (string-trim
                          (with-output-to-string (lambda () (system "date"))))))
    (render-page (redirect/get)))
  (send/suspend/dispatch response-generator))

(define (start request)
  (render-page request))


(serve/servlet start
               #:extra-files-paths (list (current-directory)))
