#lang racket

;; https://mirrors.tuna.tsinghua.edu.cn/gnu/emacs/emacs-26.1.tar.gz 65007481 62M
;; https://mirrors.tuna.tsinghua.edu.cn/gnu/emacs/emacs-26.1.tar.gz.sig 488

(require net/url)

(define (download url)
  (define filename (file-name-from-path url))
  (printf "Downloading ~a to ~a...\n" url filename)
  (define-values (status headers in) (http-sendrecv/url (string->url url)))
  (define content-length
    (string->number (bytes->string/utf-8 (extract-header headers "Content-Length"))))
  (define percentages
    (for/list ([i (in-range 10 101 10)])
      (exact-floor (* i (/ content-length 100)))))
  (call-with-output-file filename #:exists 'truncate
    (lambda (out)
      ;; XXX 阅读 copy-port 的源代码，看看更为高效的方法
      ;; (copy-port in out)
      (let loop ([i 0]
                 [byte (read-byte in)])
        (if (eof-object? byte)
            (displayln "Done")
            (begin
              (when (memv i percentages)
                (printf "~a%\n"
                        (~a (exact->inexact (* (/ i content-length) 100)) #:width 2)))
              (write-byte byte out)
              (loop (add1 i) (read-byte in))))))))

(define (extract-header headers header)
  (let/cc return
    (for ([i (in-list headers)])
      (let ([matches (regexp-match
                      ;; ignore case
                      (regexp (format "^(?i:~a): (.+)$" header))
                      i)])
        (when matches
          (return (second matches)))))))

(module+ main
  (define url (vector-ref (current-command-line-arguments) 0))
  (download url))
