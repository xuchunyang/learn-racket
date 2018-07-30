#lang racket

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l)) (lat? (cdr l))]
        (else #f)))

(lat? '(Jack Sprat could eat no chicken))
;; => #t

(lat? '((Jack) Sprat could eat no chicken fat))
;; => #f

(lat? '(Jack (Sprat could) eat no chicken fat))
;; => #f

(lat? '())
;; => #t

;; a lat is a list of atoms

(lat? '(bacon and eggs))
;; => #t

(or (null? '())
    (atom? '(d e f g)))
;; => #t

(or (null? '(a b c))
    (null? '()))
;; => #t

(member 'tea '(fried eggs and scrambled eggs))
;; => #f

(define (member? elt lst)
  (cond [(null? lst) #f]
        [else (or (eq? elt (car lst))
                  (member? elt (cdr lst)))]))

(require rackunit)
(check-true (member? 'c '(a b c)))
(check-false (member? 'c '(a b)))
(check-false (member? 'c '()))

(member? 'meat '(mashed pototoes and meat gravy))
;; => #t

