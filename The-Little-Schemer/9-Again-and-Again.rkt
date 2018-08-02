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
    (list (car (car pair))
          (cons (car (cdr (car pair)))
                (cdr pair)))))

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

(align 1)
;; => 1

(align '((1 2) 3))
;; => '(1 (2 3))

(align '(1 (2 3)))
;; => '(1 (2 3))

(align '(1 ((2 3) 4)))
;; => '(1 (2 (3 4)))

(define length*
  (lambda (pora)
    (cond [(atom? pora) 1]
          [else (+ (length* (first pora))
                   (length* (second pora)))])))

(length* 1)
;; => 1

(length* '((1 2) (3 (4 5))))
;; => 5

(length* '(1 (2 (3 (4 5)))))
;; => 5

(length* '(1 2))
;; => 2

(define weigth*
  (lambda (pora)
    (cond [(atom? pora) 1]
          [else
           (+ (* 2 (weigth* (first pora)))
              (weigth* (second pora)))])))

(weigth* '(1 2))
;; => 3

(weigth* '((1 2) 3))
;; => 7

(weigth* '(((1 2) 3) 4))
;; => 15

(weigth* '(1 (2 (3 4))))
;; => 7

;; Is align a partial function?
;; No, it yields a value for every argument


(define shuffle
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (shuffle (revpair pora))]
      [else (list (first pora)
                  (shuffle (second pora)))])))

(shuffle 1)
;; => 1

(shuffle '(1 2))
;; => '(1 2)

(shuffle '((1 2) 3))
;; => '(3 (1 2))

(shuffle '(1 (2 (3 4))))
;; => '(1 (2 (3 4)))

(shuffle '(((1 2) 3) 4))
;; => '(4 (3 (1 2)))

;; (shuffle '((a b) (c d)))

;; shuffle is not total



;; Is C total?
;;
;; Nobody knows

(define C
  (lambda (n)
    (if (= n 1)
        1
        (if (even? n)
            (C (÷ n 2))
            (C (add1 (* 3 n)))))))

#;
(for/list [(i (in-range 1 100))]
  (C i))

#|
hello
multiline
comment
|#

(define A
  (lambda (n m)
    (cond [(zero? n) (add1 m)]
          [(zero? m) (A (sub1 n) 1)]
          (else (A (sub1 n)
                   (A n (sub1 m)))))))

(A 1 0)
;; => 2

(A 1 1)
;; => 3

(A 2 2)
;; => 7

#;
(A 4 3)

;; A is total

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))

(define will-stop?
  (lambda (f)
    ;; #f or #t
    (f)))



(define length
  (lambda (l)
    (if (empty? l)
        0
        (add1 (length (rest l))))))

(define length0
  (lambda (l)
    (if (empty? l)
        0
        (error "unsupported"))))

(length0 '())
;; => 0

'(length0 '(1))

(define length1
  (lambda (l)
    (if (empty? l)
        0
        (add1 (length0 (rest l))))))

(length1 '())
;; => 0

(length1 '(1))
;; => 1

((λ (l)
   (if (empty? l)
       0
       (add1 ((λ (l)
                (if (empty? l)
                    0
                    (add1 ((λ (l)
                             (if (empty? l)
                                 0
                                 (error "here")))
                           (cdr l)))))
              (rest l))))) '(1 2))
;; => 2


(define length00
  (λ (l)
    (if (empty? l)
        0
        (add1 (eternity (rest l))))))

(λ (l)
  (if (empty? l)
      0
      (add1 ((λ (l)
               (if (empty? l)
                   0
                   (eternity (rest l))))
             (cdr l)))))

((λ (l)
   (if (empty? l)
       0
       (add1 ((λ (l)
                (if (empty? l)
                    0
                    (add1 ((λ (l)
                             (if (empty? l)
                                 0
                                 (eternity (rest l))))
                           (rest l)))))
              (cdr l))))) '(1 2))
;; => 2

;; length0
((λ (length)
   (λ (l)
     (if (empty? l)
         0
         (add1 (length (cdr l))))))
 eternity)
;; => #<procedure>

;; length≤1
((λ (length)
   (λ (l)
     (if (empty? l)
         0
         (add1 (length (cdr l))))))
 (λ (l)
   (if (empty? l)
       0
       (add1 (eternity (cdr l))))))
;; => #<procedure>

;; length≤2
((λ (length)                            ; 2
   (λ (l)
     (if (empty? l)
         0
         (add1 (length (cdr l))))))
 ((λ (length)                           ; 1
    (λ (l)
      (if (empty? l)
          0
          (add1 (length (cdr l))))))
  (λ (l)                                ; 0
    (if (empty? l)
        0
        (add1 (eternity (cdr l)))))))
;; => #<procedure>

;; length0
((λ (mk-length)
   (mk-length eternity))
 (λ (length)
   (λ (l)
     (if (empty? l)
         0
         (add1 (length (cdr l)))))))

;; length≤1
((λ (mk-length)
   (mk-length (mk-length eternity)))
 (λ (length)
   (λ (l)
     (if (empty? l)
         0
         (add1 (length (cdr l)))))))

;; length≤2
((λ (mk-length)
   (mk-length (mk-length (mk-length eternity))))
 (λ (length)
   (λ (l)
     (if (empty? l)
         0
         (add1 (length (cdr l)))))))

;; length≤3
((λ (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (λ (length)
   (λ (l)
     (if (empty? l)
         0
         (add1 (length (cdr l)))))))

;; All names are equal

;; length0
((λ (mk-length)
   (mk-length mk-length))
 (λ (length)
   (λ (l)
     (if (empty? l)
         0
         (add1 (length (cdr l)))))))

;; length0
((λ (mk-length)
   (mk-length mk-length))
 ;; Rename: length -> mk-length
 (λ (mk-length)
   (λ (l)
     (if (empty? l)
         0
         (add1 (mk-length (cdr l)))))))

;; length≤1
((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   (λ (l)
     (if (empty? l)
         0
         (add1 ((mk-length eternity)
                (cdr l)))))))

(((λ (mk-length)
    (mk-length mk-length))
  (λ (mk-length)
    (λ (l)
      (if (empty? l)
          0
          (add1 ((mk-length eternity)
                 (cdr l)))))))
 '(apple))
;; => 1

;; length
((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   (λ (l)
     (if (empty? l)
         0
         (add1 ((mk-length mk-length)
                (cdr l)))))))

#;
((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   ((λ (length)
      (λ (l)
        (if (empty? l)
            0
            (add1 (length (cdr l))))))
    (mk-length mk-length))))

;; (mk-length mk-length)
;; ->
;; (λ (x) ((mk-length mk-length) x))

((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   (λ (l)
     (if (empty? l)
         0
         (add1 ((λ (x)
                  ((mk-length mk-length) x))
                (cdr l)))))))

((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   ((λ (length)
      (λ (l)
        (if (empty? l)
            0
            (add1 (length
                   (cdr l))))))
    (λ (x)
      ((mk-length mk-length) x)))))

(((λ (le)
    ((λ (mk-length)
       (mk-length mk-length))
     (λ (mk-length)
       (le (λ (x)
             ((mk-length mk-length) x))))))
  (λ (length)
    (λ (l)
      (if (empty? l)
          0
          (add1 (length (cdr l))))))) '(1 2 3))
;; => 3

(λ (le)
  ((λ (mk-length)
     (mk-length mk-length))
   (λ (mk-length)
     (le (λ (x)
           ((mk-length mk-length) x))))))

;; The applicative-order Y combinator
(define Y
  (λ (le)
    ((λ (f) (f f))
     (λ (f)
       (le (λ (x) ((f f) x)))))))

(Y (λ (length)
     (λ (l)
       (if (empty? l)
           0
           (add1 (length (cdr l)))))))
;; => #<procedure>
