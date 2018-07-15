#lang racket/base

(require net/head
         net/smtp
         openssl
         racket/list
         racket/port
         racket/string
         racket/system
         racket/tcp)

(define (mail smtp-server smtp-port smtp-user smtp-pass
              from to subject body)
  (define header (standard-message-header from
                                          to
                                          (list)
                                          (list)
                                          subject))
  (define message
    (cond [(port? body) (port->lines body)]
          [(string? body) (port->lines
                           (open-input-string body))]
          [else (raise-argument-error 'mail "(or port? string?)" body)]))
  (parameterize ([smtp-sending-end-of-message
                  (lambda () (displayln "smtp-send-message is done!"))])
    (smtp-send-message smtp-server
                       from
                       to
                       header
                       message
                       #:port-no smtp-port
                       #:auth-user smtp-user
                       #:auth-passwd smtp-pass
                       #:tcp-connect
                       ;; https://www.sparkpost.com/blog/what-smtp-port/
                       (if (memv smtp-port '(465 587 2525))
                           ssl-connect
                           tcp-connect))))

(define password
  (call-with-input-file (expand-user-path "~/.authinfo")
    (lambda (in)
      (let/cc return
        (for ([s (in-lines in)])
          (let ((match (regexp-match #rx"machine smtp.yandex.com .*password ([^ ]+)"
                                     s)))
            (when match
              (return (second match)))))))))

(mail "smtp.yandex.com" 465 "mail@xuchunyang.me" password
      "mail@xuchunyang.me" (list "xuchunyang56@163.com")
      (format "Send mail in Racket (~a)"
              (string-trim (with-output-to-string (lambda () (system "date")))))
      #<<EOF
Hello,

How are you?

--
Send with Racket
EOF
      )
