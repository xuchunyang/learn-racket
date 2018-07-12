;; https://docs.racket-lang.org/continue/index.html
#lang web-server/insta

;; TODO: 持久化

(struct blog (posts) #:mutable)

(struct post (title body))

(define BLOG
  (blog
   (list (post "Fist Post!"
               "Hey, this is my first post!")
         (post "Hello Racket"
               "I'm learning Racket")
         (post "Yet Another Post"
               "..."))))

(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))

(define (start request)
  (render-blog-page request))

(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))

(define (render-blog-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "My Blog"))
       (body (h1 "My Blog")
             ,(render-posts)
             (form
              ((action
                ,(embed/url insert-post-handler)))
              (input ((name "title")))
              (input ((name "body")))
              (input ((type "submit"))))))))
  (define (insert-post-handler request)
    (blog-insert-post!
     BLOG (parse-post (request-bindings request)))
    ;; Fix double-submission problem
    (render-blog-page (redirect/get)))
  (send/suspend/dispatch response-generator))

(define (render-post post)
  `(div ((class "post"))
        (h2 ,(post-title post))
        (p ,(post-body post))))

(define (render-posts)
  `(div ((class "posts"))
        ,@(map render-post (blog-posts BLOG))))
