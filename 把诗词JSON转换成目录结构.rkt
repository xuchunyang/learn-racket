#lang racket

(require json)

(define (extract-json json-file)
  (with-input-from-file json-file
    read-json))

(define (save-poet author title paragraphs)
  (cond
    [(equal? "" title)
     (displayln "标题为空")
     (displayln author)
     (displayln paragraphs)]
    [(or (string-contains? author "/")
         (string-contains? title "/"))
     (printf "作者名或标题包含非法字符\n~a\nBy ~a\n"
             title author)]
    [(> (string-length title) 100)
     (printf "标题太长:\n~a\nBy ~a\n"
             title author)]
    [else
     (let ([filename (build-path author title)])
       (unless (file-exists? filename)
         (make-directory* author)
         (with-output-to-file filename
           (lambda () (map displayln paragraphs)))))]))

(define (save-hash h)
  (save-poet (hash-ref h 'author)
             (hash-ref h 'title)
             (hash-ref h 'paragraphs)))

(define (save-hash-ci h)
  (save-poet (hash-ref h 'author)
             ;; 词牌名
             (hash-ref h 'rhythmic)
             (hash-ref h 'paragraphs)))

(module+ main
  (unless (directory-exists? "诗词")
    (make-directory "诗词"))
  (current-directory "诗词")

  ;; 诗
  (let ([DIR (expand-user-path "~/src/chinese-poetry/json/")])
    (unless (directory-exists? DIR)
      (error 'foo "目录 ~a 不存在，请从 https://github.com/chinese-poetry/chinese-poetry 下载" DIR))
    (for ([f (in-directory DIR)])
      (when (regexp-match
             ;; poet.song.25000.json
             ;; poet.tang.8000.json
             "poet\\.(?:tang|song).+\\.json"
             (path->string f))
        (printf "Processing ~a\n" f)
        (for ([h (in-list (extract-json f))])
          (save-hash h)))))

  ;; 词
  (let ([DIR (expand-user-path "~/src/chinese-poetry/ci/")])
    (unless (directory-exists? DIR)
      (error 'foo "目录 ~a 不存在，请从 https://github.com/chinese-poetry/chinese-poetry 下载" DIR))
    (for ([f (in-directory DIR)])
      (when (regexp-match
             ;; ci.song.0.json
             "ci\\.(?:tang|song).+\\.json"
             (path->string f))
        (printf "Processing ~a\n" f)
        (for ([h (in-list (extract-json f))])
          (save-hash-ci h))))))
