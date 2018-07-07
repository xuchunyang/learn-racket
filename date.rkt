#lang racket/base

(require racket/string)

(define (initials str)
  (apply string (map (lambda (s) (string-ref s 0)) (string-split str))))

(module+ test
  (require rackunit)
  (check-equal? (initials "China Standard Time") "CST"))

;; $ date
;; Sat Jul  7 19:51:42 CST 2018
;;
;; (~t (now/moment) "E MMM  d HH:mm:ss zzzz y")
;; => "Sat Jul  7 20:05:50 China Standard Time 2018"

(require gregor)
(define (the-date)
  (let ((t (now/moment)))
    (let ([prefix (~t t "E MMM  d HH:mm:ss")]
          [timezone (~t t "zzzz")]
          [year (~t t "y")])
      (string-join (list prefix (initials timezone) year)))))

(module+ test
  (require racket/port)
  (require racket/system)
  (check-equal?
   (string-trim (with-output-to-string (lambda () (system "date"))))
   (the-date)))

(module+ main
  (displayln (the-date)))
