#lang racket

(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(define +
  (λ (x y)
    (cond [(zero? y) x]
          [else (add1 (+ x (sub1 y)))])))

(+ 2 3)
;; => 5

(define *
  (λ (x y)
    (cond [(zero? y) 0]
          [else (+ x (* x (sub1 y)))])))

(* 2 3)
;; => 6

(define ↑
  (λ (a n)
    (cond [(zero? n) 1]
          [else (* a (↑ a (sub1 n)))])))

(↑ 2 3)
;; => 8

(define numbered?
  (λ (aexp)
    (cond [(atom? aexp) (number? aexp)]
          [else
           (and (numbered? (car aexp))
                (numbered? (car (cdr (cdr aexp)))))])))

(numbered? '(1 + 2))
;; => #t

(numbered? 3)
;; => #t



(define operator
  (λ (aexp)
    (car (cdr aexp))))

(define 1st-sub-exp
  (λ (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (λ (aexp)
    (car (cdr (cdr aexp)))))

(define value
  (λ (nexp)
    (cond [(atom? nexp) nexp]
          [(eq? (operator nexp) (quote +))
           (+ (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp)))]
          [(eq? (operator nexp) (quote *))
           (* (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp)))]
          [(eq? (operator nexp) (quote ↑))
           (↑ (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp)))])))

(value 13)
;; => 13

(value '(1 + 3))
;; => 4


;; (value '(+ 1 3))
;; => 4

(value '(1 + (2 * (3 ↑ 2))))
;; => 19

;; (value '(+ 1 (* 2 (↑ 3 2))))
;; => 19



;; 用 (() () ()) 表示数字 3
(module representation racket
  (define sero?
    (λ (n)
      (null? n)))

  (require rackunit)
  (check-true (sero? '()))
  (check-false (sero? '(() () ())))
  
  (define edd1
    (λ (n)
      (cons (quote ()) n)))

  (check-equal? '(()) (edd1 '()))
  
  (define zub1
    (λ (n)
      (cdr n)))

  (check-equal? '(()) (zub1 '(() ()))
                "2 - 1 = 1")

  (define +
    (λ (m n)
      (cond [(sero? n) m]
            [else (edd1 (+ m (zub1 n)))])))

  (check-equal? (+ '(() ())
                   '(() () ()))
                '(() () () () ())
                "2 + 3 = 5")

  (check-equal? (+ '(()) '()) '(()) "1 + 0 = 1")
  
  (define *
    (λ (m n)
      (cond [(sero? n) (quote ())]
            [else (+ m (* m (zub1 n)))])))

  (check-equal? (* '(() ())
                   '(() () ()))
                '(() () () () () ())
                "2 * 3 = 6"))

;; Shadow 什么意思？
