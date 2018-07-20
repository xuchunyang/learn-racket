#lang racket

(require html-parsing
         net/url
         sxml
         openssl
         net/head
         net/smtp)

(struct story (title url) #:prefab)

;; 返回 HN 上所有提及 Emacs 的主题，如果一个也没有，返回 '()
(define (mentions-emacs)
  (define-values (status headers html)
    (http-sendrecv/url (string->url "https://news.ycombinator.com/")))
  (define xexp (html->xexp html))
  (define stories
    (map
     (lambda (node)
       (let ((top (cons '*TOP* (list node))))
         (story (car ((sxpath "/a/text()") top))
                (car ((sxpath "/a/@href/text()") top)))))
     ((sxpath "//a[@class='storylink']") xexp)))
  (filter (lambda (a-story) (regexp-match #rx"(?i:emacs)" (story-title a-story)))
          stories))

(define (mail-me subject body)
  (define from "if-hacker-news-mentions-emacs-then-mail-me.rkt <xu.chunyang@icloud.com>")
  (define to (list "Myself <xu.chunyang@icloud.com>"))
  (define header (standard-message-header
                  from
                  to
                  '()
                  '()
                  subject))
  (parameterize ([smtp-sending-end-of-message
                  (lambda () (printf "mail '~a' sent!\n" subject))])
    (smtp-send-message "smtp.mail.me.com"
                       from
                       to
                       header
                       ;; body should be a string
                       (string-split body "\n")
                       #:port-no 587
                       #:auth-user "xu.chunyang@icloud.com"
                       #:auth-passwd (get-password)
                       #:tls-encode ports->ssl-ports)))

(define (get-password)
  (call-with-input-file (expand-user-path "~/.authinfo")
    (lambda (in)
      (let/cc return
        (for ([s (in-lines in)])
          (let ((match (regexp-match "machine smtp.mail.me.com .*password ([^ ]+)"
                                     s)))
            (when match
              (return (second match)))))))))

(module+ test
  (require rackunit)
  (test-case "忽略大小写"
    (check-not-false (regexp-match #rx"(?i:emacs)" "emacs"))
    (check-not-false (regexp-match #rx"(?i:emacs)" "Emacs"))
    (check-not-false (regexp-match #rx"(?i:emacs)" "EMACS")))

  (test-case "获得密码"
    (check-true (string? (get-password)))))

(define (shell-command-to-string command)
  (string-trim (with-output-to-string (lambda () (system command)))))

(module+ test
  (check-equal? (shell-command-to-string "echo hello") "hello"))

(module+ main
  (let loop ([new-stories (mentions-emacs)]
             [old-stories empty])
    (define really-new-storis
      (filter (lambda (a-story) (not (member a-story old-stories)))
              new-stories))
    (define now (shell-command-to-string "date"))
    (cond [(empty? new-stories)
           (printf "[~a] Hacker News 上没有提及 Emacs 的主题\n" now)]
          [(empty? really-new-storis)
           (printf "[~a] Hacker News 上没有提及 Emacs 的新主题\n" now)]
          [else
           (printf "[~a] 发现主题，准备发送邮件...\n"
                   (shell-command-to-string "date"))])
    (map (lambda (a-story)
           (mail-me (story-title a-story)
                    (story-url a-story)))
         really-new-storis)
    (printf "[~a] sleep...\n"
            (shell-command-to-string "date"))
    (sleep 300)
    (loop (mentions-emacs) (append really-new-storis old-stories))))
