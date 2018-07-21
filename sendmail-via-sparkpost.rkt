;; SparkPost 提供了每月十万封的免费额度，个人用绰绰有余
;;
;; 利用 SparkPost 的 HTTP API 发送邮件

#lang racket

;; 我保存到了 LastPass 中了
;; $ lpass show 'SparkPost API Key'
;;
;; 如果丢失，到 https://app.sparkpost.com/account/api-keys 重现生成

(define token (begin (display "API Token: ")
                     (read-line)))

(require net/url)
(require json)

(define-values (status headers out)
  (http-sendrecv/url
   (string->url
    ;; https://developers.sparkpost.com/api/
    "https://api.sparkpost.com/api/v1/transmissions")
   #:method #"POST"
   #:headers (list (format "Authorization: ~a" token)
                   "Content-Type: application/json")
   #:data (with-output-to-string
            (lambda ()
              (write-json
               (hasheq 'recipients (list (hasheq 'address "xu.chunyang@icloud.com"))
                       'content (hasheq 'from "racket@sparkpost.xuchunyang.me"
                                        'subject "sendmail-via-sparkpost.rkt 1111"
                                        'text "似此星辰非昨夜，为谁风露立中宵。 -- 黄景仁《绮怀》")))))))

;; json response
(displayln (port->string out))
