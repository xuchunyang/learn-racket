#lang racket/base

(require rackunit)

;; car
(check-exn
 exn:fail:contract?
 (lambda () (car null)))

;; cdr

(check-exn
 exn:fail:contract?
 (lambda () (cdr null)))

;; cons

(check-equal? (list 1) (cons 1 null))

;; null?

(check-true (null? null))

;; eq?

(check-false (eq? '(1 2) '(1 2)))
(check-true (eq? 'foo 'foo))
