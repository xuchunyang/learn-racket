#lang racket

;; ---------------------------------------------------------------
;; Expect

(let ([it 3])
  (if it
      (+ it 100)
      #f))
;; => 103

(aif 3 (+ it 3) #f)

;; ---------------------------------------------------------------
;; Incorrect way

(define-syntax (aif stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(let ([it condition])
         (if it
             true-expr
             false-expr))]))

(define-syntax-rule (aif condition true-expr false-expr)
  (let ([it condition])
    (if it
        true-expr
        false-expr)))

(aif 3 (+ it 3) #f)

;; ---------------------------------------------------------------

(define current-foo (make-parameter "some default value"))
(current-foo)
;; => "some default value"
(parameterize ([current-foo "I have a new value, for now"])
  (current-foo))
;; => "I have a new value, for now"
(current-foo)
;; => "some default value"

(require racket/stxparam)
(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside aif")))

(define-syntax-rule (aif condition true-expr false-expr)
  (let ([tmp condition])
    (if tmp
        (syntax-parameterize ([it (make-rename-transformer #'tmp)])
          true-expr)
        false-expr)))

(aif 10 (displayln it) (void))
;; => 10

(aif #f (displayln it) (void))
