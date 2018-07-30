#lang racket/base

(require rackunit)

;; The Law of Car
;;
;; The primitive car is defined only for non-empty lists.

(check-exn
 exn:fail:contract?
 (lambda () (car null)))

;; The Law of Cdr
;;
;; The primitive cdr is defined only for non-empty list.  The cdr of
;; any non-empty list is always another list.

(check-exn
 exn:fail:contract?
 (lambda () (cdr null)))

;; The Law of Cons
;;
;; The primitive cons takes two arguments.  The second argument to
;; cons must be a list.  The result is a list.

(check-equal? (list 1) (cons 1 null))

;; The Law of Null?
;;
;; The primitive null? is defind only for lists.

(check-true (null? null))

;; The Law of eq?
;;
;; The primitive eq? takes two arguments.  Each must be a non-numeric
;; atom.

(check-false (eq? '(1 2) '(1 2)))
(check-true (eq? 'foo 'foo))
