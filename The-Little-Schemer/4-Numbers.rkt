#lang racket

(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(atom? 14)
;; => #t

(atom? 14)
;; => #t

(number? -3)
;; => #t

(number? 3.14159)

(add1 67)
;; => 68

(sub1 5)
;; => 4

(sub1 0)
;; => -1

(zero? 0)
;; => #t

(zero? 1492)
;; => #f

(+ 46 12)
;; => 58

(define plus
  (λ (n m)
    (cond [(zero? m) n]
          [else (add1 (plus n (sub1 m)))])))

(plus 2 3)
;; => 5

(define minus
  (λ (n m)
    (cond [(zero? m) n]
          [else (sub1 (minus n (sub1 m)))])))

(minus 3 2)
;; => 1

;; (- n m)
;;
;; It takes two numbers as arguments, and reduces the second until it
;; hits zero. It subtracts one from the result as many times as it did
;; to causet the second one to reach zero.

(define tup?
  (λ (l)
    (cond [(null? l) #t]
          [else (cond [(number? (car l))
                       (tup? (cdr l))]
                      [else #f])])))

(tup? '(2 11 3 79 47 6))
;; => #t

(tup? '(8 55 5 555))
;; => #t

(tup? '(1 2 8 apple 4 3))
;; => #f

(tup? '(3 (7 4) 13 9))
;; => #f

(tup? '())
;; => #t

(define addtup
  (λ (tup)
    (cond [(null? tup) 0]
          [else (+ (car tup) (addtup (cdr tup)))])))

(addtup '(3 5 2 8))
;; => 18

(addtup '(15 6 7 12 3))
;; => 43

(define X
  (λ (n m)
    (cond [(zero? m) 0]
          [else (+ n (X n (sub1 m)))])))

(X 5 3)
;; => 15

(X 13 4)
;; => 52



(define tup+
  (λ (tup1 tup2)
    (cond [(null? tup1) tup2]
          [(null? tup2) tup1]
          [else (cons (+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))])))

(tup+ '(3 6 9 11 4)
      '(8 5 2 0  7))
;; => '(11 11 11 11 11)

(tup+ '(2 3)
      '(4 6))
;; => '(6 9)

(require rackunit)
(tup+ '(3 6 9 11 4)
      '(8 5))

;; What does (tup+ tup1 tup2) do?
;;
;; It adds the first number of tup1 to the first number of tup2, then
;; adds the second number of tup1 to the second number of tup2, and so
;; on, building a tup of the anwsers, for tups of the same length.



(define >
  (λ (n m)
    (cond [(zero? n) #f]
          [(zero? m) #t]
          [else (> (sub1 n) (sub1 m))])))

(check-true (> 3 2))
(check-false (> 3 3))
(check-false (> 2 3))



(define <
  (lambda (n m)
    (cond [(zero? m) #f]
          [(zero? n) #t]
          [else (< (sub1 n) (sub1 m))])))

(< 2 3)
;; => #t

(< 3 3)
;; => #f

(< 3 2)
;; => #f



(define =
  (λ (n m)
    (cond
      [(zero? m) (zero? n)]
      [(zero? n) #f]
      [else (= (sub1 n) (sub1 m))])))

(= 3 3)
;; => #t

(= 1 3)
;; => #f

(= 0 4)
;; => #f



(define ↑
  (λ (n m)
    (cond [(zero? m) 1]
          [else (* n (↑ n (sub1 m)))])))


(↑ 1 1)
;; => 1

(↑ 2 3)
;; => 8

(↑ 5 3)
;; => 125

(define ÷
  (λ (n m)
    (cond [(< n m) 0]
          [else (add1 (÷ (- n m) m))])))

(÷ 15 4)
;; => 3



(define length
  (λ (lat)
    (cond [(null? lat) 0]
          [else (add1 (length (cdr lat)))])))

(length '(hotdogs with mustard sauerkraut and pickles))
;; => 6

(length '(ham and chess on rye))
;; => 5



(define pick
  (λ (n lat)
    (cond [(zero? (sub1 n) (car lat))]
          [else (pick (sub1 n) (cdr lat))])))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))
;; => 'macaroni



(define rempick
  (λ (n lat)
    (cond [(zero? (sub1 n)) (cdr lat)]
          [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

(rempick 3 '(hotdogs with hot mustard))
;; => '(hotdogs with mustard)



(define no-nums
  (λ (lat)
    (cond [(null? lat) '()]
          [else (cond [(number? (car lat))
                       (no-nums (cdr lat))]
                      [else
                       (cons (car lat)
                             (no-nums (cdr lat)))])])))

(no-nums '(5 pears 6 prunes 9 dates))
;; => '(pears prunes dates)



(define all-nums
  (λ (lat)
    (cond [(null? lat) '()]
          [else
           (cond [(number? (car lat))
                  (cons (car lat)
                        (all-nums (cdr lat)))]
                 [else
                  (all-nums (cdr lat))])])))

(all-nums '(5 pears 6 prunes 9 dates))
;; => '(5 6 9)



(define eqan?
  (λ (a1 a2)
    (cond [(and (number? a1) (number? a2))
           (= a1 a2)]
          [(or (number? a1) (number? a2))
           #f]
          [else (eq? a1 a2)])))



(define occur
  (λ (a lat)
    (cond [(null? lat) 0]
          [else (cond [(eq? a (car lat))
                       (add1 (occur a (cdr lat)))]
                      [else (occur a (cdr lat))])])))



(define one?
  (λ (n) (= n 1)))

(define rempick2
  (λ (n lat)
    (cond [(one? n) (cdr lat)]
          [else (cons (car lat)
                      (rempick (sub1 n)
                               (cdr lat)))])))

(rempick2 3 '(lemon meringue salty pie))
;; => '(lemon meringue pie)

