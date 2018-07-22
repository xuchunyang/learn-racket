#lang racket
(require web-server/http
         web-server/managers/lru
         web-server/servlet)

(provide interface-version manager start)
 
(define interface-version 'v2)

(define manager
  (make-threshold-LRU-manager #f (* 1024 1024 128)))

(struct post (content timestamp) #:prefab)

(define POSTS (list (post "二十六冷，二十七热。" (current-seconds))
                    (post "吃瓜不吐籽，籽比瓜脆。" (current-seconds))))

;; TODO: Display timestamp
(define (render-posts)
  `(ol
    ,@(map (λ (a-post) `(li ,(post-content a-post))) POSTS)))

(define (remove-nth lst nth)
  (for/fold ([acc '()]
             #:result (reverse acc))
            [(i (in-naturals))
             (x (in-list lst))]
    (if (= i nth)
        acc
        (cons x acc))))

(define (render-page req)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (body (h1 "胡言乱语")
                  ,(render-posts)
                  (h2 "添加")
                  (form
                   ((action
                     ,(embed/url insert-post-handler)))
                   (input ((name "content")))
                   (input ((type "submit"))))
                  (h2 "删除")
                  (form
                   ((action
                     ,(embed/url delete-post-handler)))
                   (input ((name "id") (placeholder "输入编号")))
                   (input ((type "submit"))))))))
  (define (insert-post-handler req)
    (define bindings (request-bindings req))
    (define content (extract-binding/single 'content bindings))
    (unless (equal? content "")
      (define a-post (post (extract-binding/single 'content bindings)
                           (current-seconds)))
      (set! POSTS (append POSTS (list a-post))))
    (render-page (redirect/get)))
  (define (delete-post-handler req)
    (define bindings (request-bindings req))
    (define id (string->number (extract-binding/single 'id bindings)))
    (when id
      (set! POSTS (remove-nth POSTS (sub1 id))))
    (render-page (redirect/get)))
  (send/suspend/dispatch response-generator))

(define (start req)
  (render-page req))

(module+ main
  (require web-server/servlet-env)
  (serve/servlet start))
