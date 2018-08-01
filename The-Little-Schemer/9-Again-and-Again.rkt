#lang racket

(require "lib.rkt")

;; 6 -> 7 -> 3 -> foo -> #t
'(looking 'foo '(6 2 4 foo 5 7 3))

;; 6 -> 7 -> 3 -> bar -> #f
'(looking 'foo '(6 2 bar foo 5 7 3))

(define looking
  (λ (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  ;; The name sorn stands for "Symbol or Number"
  (λ (a sorn lat)
    (cond [(number? (pick sorn lat))
           (keep-looking a (pick sorn lat) lat)]
          [else (eq? a (pick sorn lat))])))

(looking 'foo '(6 2 4 foo 5 7 3))
(looking 'foo '(6 2 bar foo 5 7 3))

;; keep-looing is an "unnatural" recursion function



;; the function shift takes a pair whose first component is a pair and
;; builds a pair by shifting the second part of the first component
;; into the second component.

(define shift
  (λ (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(require rackunit)
(check-equal? (shift '((a b) c)) '(a (b c)))
(check-equal? (shift '((a b) (c d))) '(a (b (c d))))



(define align
  (λ (pora)
    (cond [(atom? pora) pora]
          [(a-pair? (first pora))
           (align (shift pora))]
          [else (build (first pora)
                       (align (second pora)))])))

(+ (* (f '(a b)) 2)
   1)

(+ (* (+ (* 1 2)
         1) 2)
   1)
;; => 7

;; partial function
;; total function
