#lang racket

(define DB "persistent-data.dat")

(define (write-to-DB data)
  (with-output-to-file DB
    #:exists 'replace
    (lambda () (write data))))

(define (read-from-DB)
  (with-input-from-file DB read))

(module+ test
  (require rackunit)
  (test-case "数字"
    (define PI 3.14)
    (write-to-DB PI)
    (check-equal? PI (read-from-DB)))

  (test-case "Struct"
    (struct dot (x y) #:prefab)
    (define d (dot 1 2))
    (write-to-DB d)
    ;; #:prefab 比 #:transparent 还透明，可以直接比较是否相同
    (check-equal? d (read-from-DB))))
