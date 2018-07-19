#lang racket
(require net/head
         net/imap
         net/unihead)

(define (get-password-from-netrc netrc machine login)
  (call-with-input-file (expand-user-path netrc)
    (lambda (in)
      (let/cc return
        (for ([s (in-lines in)])
          (let ((match (regexp-match (format "machine ~a .*password ([^ ]+)" machine)
                                     s)))
            (when match
              (return (second match)))))))))

(define (latest server username password)
  (define-values (conn total recent)
    (imap-connect server username password "INBOX"))
  (define messages (imap-get-messages conn (list total) '(header body)))
  (match-define (list header body) (car messages))
  (define subject (decode-for-header (bytes->string/utf-8
                                      (extract-field #"Subject" header))))
  ;; Print result
  (displayln subject)
  (newline)
  (display body))

(module+ main
  (define server "imap.yandex.com")
  (define username "mail@xuchunyang.me")
  (define password (get-password-from-netrc "~/.authinfo" server username))
  (latest server username password))
