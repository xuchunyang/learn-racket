#lang racket
(require db
         web-server/http
         web-server/managers/none)
(provide interface-version manager start)
(define interface-version 'v2)
(define manager
  (create-none-manager
   (lambda (req)
     (response/xexpr
      `(html (head (title "No Continuations Here!"))
             (body (h1 "No Continuations Here!")))))))

(define (start req)
  (define bindings (request-bindings/raw req))
  (define (get-value id bindings)
    (match (bindings-assq id bindings)
      [(? binding:form? b)
       (bytes->string/utf-8 (binding:form-value b))]
      [_ #f]))
  (define author (get-value #"author" bindings))
  (define title (get-value #"title" bindings))
  (define footer '(p "Powered by " (a ((href "https://racket-lang.org/")) "Racket")))
  (if (and author title)
      (let ([author+title (string-append author " - " title)])
        (response/xexpr
         `(html (head (title ,author+title))
                (body (h2 ,author+title)
                      ,(let ((res (search author title)))
                         (if res
                             `(pre ,res)
                             "没找到"))
                      ,footer))))
      (response/xexpr
       `(html (head (title "诗词"))
              (body (h2 "诗词")
                    (form
                     (label ((for "author")) "作者：")
                     (input ((type "text") (name "author") (required "") (value "李清照")))
                     (label ((for "title")) "标题：")
                     (input ((type "text") (name "title") (required "") (value "声声慢")))
                     (input ((type "submit") (value "搜索"))))
                    ,footer)))))

(define DB
  (if (file-exists? "poetry.sqlite")
      "poetry.sqlite"
      ;; Ubuntu VPS
      "/var/www/xuchunyang.me/racket/poetry.sqlite"))

(define db-conn
  (virtual-connection
   (connection-pool
    (lambda () (sqlite3-connect #:database DB #:mode 'read-only)))))

(define (search author title)
  (define rows (query-rows
                db-conn
                "select * from poetry where author = $1 and title = $2;"
                author title))
  (if (empty? rows)
      #f
      (last (vector->list (first rows)))))

;; 测试
(module+ main
  (require web-server/servlet-env)
  (serve/servlet start))
