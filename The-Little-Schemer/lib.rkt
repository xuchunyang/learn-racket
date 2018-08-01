#lang racket

(provide atom? + * ↑ first second third build)

(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(module+ test
  (require rackunit)
  (check-true (atom? 42))
  (check-false (atom? '()))
  (check-false (atom? '(42)))
  (check-false (atom? '(a . b))))

;; NOTE 数值运算仅支持正整数

(define +
  (λ (x y)
    (cond [(zero? y) x]
          [else (add1 (+ x (sub1 y)))])))

(module+ test
  (check-equal? (+ 3 0) 3)
  (check-equal? (+ 3 4) 7)
  (check-equal? (+ 4 3) 7))

(define *
  (λ (x y)
    (cond [(zero? y) 0]
          [else (+ x (* x (sub1 y)))])))

(module+ test
  (check-equal? (* 3 0) 0)
  (check-equal? (* 3 4) 12))

;; a^n
(define ↑
  (λ (a n)
    (cond [(zero? n) 1]
          [else (* a (↑ a (sub1 n)))])))

(module+ test
  (check-equal? (↑ 2 0) 1)
  (check-equal? (↑ 2 10) 1024))

(define first
  (λ (l)
    (car l)))

(define second
  (λ (l)
    (car (cdr l))))

(define third
  (λ (l)
    (car (cdr (cdr l)))))

(define f
  (λ (x) x))

((f +) 1 2)

(define build (λ (s1 s2) (cons s1 (cons s2 (quote ())))))
