#lang racket/base

(require html-parsing
         net/url
         racket/file
         racket/list
         racket/path
         racket/port
         sxml)

;; https://imgs.xkcd.com/comics/stargazing_2.png
(define (download-image url)
  (make-directory* "xkcd")
  (define filename (build-path "xkcd" (file-name-from-path url)))
  (unless (file-exists? filename)
    (define-values (status headers in) (http-sendrecv/url (string->url url)))
    (call-with-output-file filename #:exists 'truncate
      (lambda (out)
        (copy-port in out)))))

;; XXX timeout? proxy? thread?
(let loop ([i 1]
           [url "https://xkcd.com/"])
  ;; XXX 如果下载所有的漫画，使用结束条件
  ;; (string-suffix? "https://xkcd.com/1/#" "#")
  (unless (i . > . 10)
    (printf "Downloading ~a...\n" url)
    (let-values ([(status headers html)
                  (http-sendrecv/url (string->url url))])
      (define xexp (html->xexp html))
      (define imgurl
        (string-append "https:"
                       ;; //imgs.xkcd.com/comics/stargazing_2.png
                       (cadr (car ((sxpath "//*[@id='comic']/img/@src") xexp)))))
      (define prevurl
        (string-append "https://xkcd.com/"
                       ;; /2016/
                       (cadr (car ((sxpath "//a[@rel='prev']/@href") xexp)))))
      (printf "Downloading ~a...\n" imgurl)
      (download-image imgurl)
      (loop (add1 i) prevurl))))
