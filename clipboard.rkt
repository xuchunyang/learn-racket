#lang racket

(provide copy paste)

(define (macosx-copy str)
  (with-input-from-string str
    (lambda () (system* (find-executable-path "pbcopy")))))

(define (macosx-paste)
  (with-output-to-string
    (lambda () (system* (find-executable-path "pbpaste")))))

(define (copy str)
  (if (eq? (system-type) 'macosx)
      (macosx-copy str)
      (displayln "Unsupported system")))

(define (paste)
  (if (eq? (system-type) 'macosx)
      (macosx-paste)
      (displayln "Unsupported system")))

(module+ test
  (require rackunit)
  (define str "some random text")
  (copy str)
  (check-equal? (paste) str))
