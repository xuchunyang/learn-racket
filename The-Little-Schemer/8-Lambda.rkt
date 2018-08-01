#lang racket

(define rember-f
  (lambda (test? a l)
    (cond [(null? l) '()]
          [(test? a (car l))
           (rember-f test? a (cdr l))]
          [else
           (cons (car l) (rember-f test? a (cdr l)))])))

(rember-f = 5 '(6 2 5 3))
;; => '(6 2 3)

(rember-f eq? 'jelly '(jelly beans are good))
;; => '(beans are good)

(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
;; => '(lemonade and (cake))

(module curry racket
  (define rember-f
    (lambda (test?)
      (lambda (a l)
        (cond [(null? l) (quote ())]
              [(test? a (car l))
               ((rember-f test?) a (cdr l))]
              [else
               (cons (car l)
                     ((rember-f test?) a (cdr l)))]))))

  (define rember-= (rember-f =))
  (define rember-eq? (rember-f eq?))
  (define rember-equal? (rember-f equal?))

  (rember-= 3 '(1 2 3 4))
  (rember-eq? 'c '(a b c d))
  (rember-equal? '(3 4) '(() 1 2 (3 4) 5)))

(module insertL-R racket
  (define insertL-f
    (lambda (test?)
      (lambda (new old l)
        (cond [(null? l) (quote ())]
              [(test? old (car l))
               (cons new
                     (cons old
                           ((insertL-f test?) new old (cdr l))))]
              [else
               (cons (car l) ((insertL-f test?) new old (cdr l)))]))))

  ((insertL-f eq?) 'A 'a '(1 a b a 1))
  ((insertL-f =) 100 1 '(1 2 3 1 4))
  ((insertL-f equal?) '(hello world) 'hi '(hi hello world hi))

  (define insertR-f
    (lambda (test?)
      (lambda (new old l)
        (cond [(null? l) (quote ())]
              [(test? old (car l))
               (cons old (cons new
                               ((insertR-f test?) new old (cdr l))))]
              [else
               (cons (car l) ((insertR-f test?) new old (cdr l)))]))))

  ((insertR-f eq?) 'A 'a '(1 a b a 1))
  ((insertR-f =) 100 1 '(1 2 3 1 4))
  ((insertR-f equal?) '(hello world) 'hi '(hi hello world hi)))

(module insert-g--my-first-attempt racket
  (define insert-g
    (lambda (test? left?)
      (lambda (new old l)
        (cond [(null? l) (quote ())]
              [(test? old (car l))
               (cond [left? (cons new
                                  (cons old
                                        ((insert-g test? left?) new old (cdr l))))]
                     [else (cons old
                                 (cons new
                                       ((insert-g test? left?) new old (cdr l))))])
               ]
              [else
               (cons (car l) ((insert-g test? left?) new old (cdr l)))]))))

  ((insert-g = #t) '=> 2 '(1 2 3 4 3 2 1))
  ((insert-g = #f) '<= 2 '(1 2 3 4 3 2 1)))


(module insert-g-seqL racket
  (define seqL
    (lambda (new old l)
      (cons new (cons old l))))

  (define seqR
    (lambda (new old l)
      (cons old (cons new l))))

  (define insert-g
    (lambda (seq)
      (lambda (new old l)
        (cond [(null? l) '()]
              [(eq? old (car l))
               (seq new old ((insert-g seq) new old (cdr l)))]
              [else
               (cons (car l) ((insert-g seq) new old (cdr l)))]))))

  (define insertL
    (insert-g (lambda (new old l) (cons new (cons old l)))))

  (insertL '-> 2 '(1 2 3))

  (define insertR
    (insert-g (lambda (new old l) (cons old (cons new l)))))

  (insertR '<- 2 '(1 2 3)))


(module insert-g racket
  (define seqL
    (lambda (new old l)
      (cons new (cons old l))))

  (define seqR
    (lambda (new old l)
      (cons old (cons new l))))

  (define seqS
    (lambda (new old l)
      (cons new l)))

  (define seqD
    (lambda (new old l)
      l))

  (define insert-g
    (lambda (seq)
      (lambda (new old l)
        (cond [(null? l) '()]
              [(eq? old (car l))
               (seq new old (cdr l))]
              [else
               (cons (car l) ((insert-g seq) new old (cdr l)))]))))

  (define subst (insert-g (lambda (new old l) (cons new l))))

  (subst 100 1 '(0 1 2 3))
  
  (define insertL (insert-g (lambda (new old l) (cons new (cons old l)))))

  (insertL 100 1 '(0 1 2 3))
  
  (define insertR (insert-g (lambda (new old l) (cons old (cons new l)))))

  (insertR 100 1 '(0 1 2 3))
  
  (define rember (insert-g (lambda (new old l) l)))

  (rember #f 1 '(0 1 2 3)))

(module value racket
  (require "lib.rkt")
  (define atom-to-function
    (λ (x)
      (cond [(eq? x '+) +]
            [(eq? x '*) *]
            [else ↑])))
  (define value
    (λ (nexp)
      (cond [(atom? nexp) nexp]
            [else
             ((atom-to-function
               (first nexp))
              (value (second nexp))
              (value (third nexp)))])))

  (require rackunit)
  (check-equal? (value '(+ 1 (* 2 (↑ 3 2)))) 19)
  (check-equal? (value 19) 19))

(module multirember racket
  (require "lib.rkt")
  (define multirember-f
    (lambda (test?)
      (lambda (a lat)
        (cond [(null? lat) (quote ())]
              [(test? a (car lat))
               ((multirember-f test?) a (cdr lat))]
              [else
               (cons (car lat)
                     ((multirember-f test?) a (cdr lat)))]))))

  (require rackunit)
  (check-equal? ((multirember-f eq?) 'b '(a b c a b c))
                '(a c a c))
  (check-equal? ((multirember-f =) 2 '(1 2 3 1 2 3))
                '(1 3 1 3))

  (define multiremberT
    (lambda (test? lat)
      (cond [(null? lat) (quote ())]
            [(test? (car lat))
             (multiremberT test? (cdr lat))]
            [else
             (cons (car lat) (multiremberT test? (cdr lat)))])))

  (multiremberT (lambda (x) (= x 2))
                '(1 2 3 3 2 1))

  (multiremberT (λ (x) (eq? x 'b))
                '(a b c a b c)))


(require "lib.rkt")

(define f
  (λ (a lat col)
    (cond [(null? lat) (col '() '())]
          [(eq? (car lat) a)
           (f a (cdr lat)
              (λ (newlat seen)
                (col newlat
                     (cons (car lat) seen))))]
          [else
           (f a (cdr lat)
              (λ (newlat seen)
                (col (cons (car lat) newlat)
                     seen)))])))

(f
 'b
 '(a b c a b c)
 (λ (newlat seen)
   (cons newlat (cons seen '()))))
;; => '((a c a c) (b b))

(f 'b '() (λ (x y) (null? y)))
;; => #t

;; 分析
;;
'(f 2 '(1 2 3) col)

'(f 2 '(2 3) (λ (newlat seen)
              (col (cons 1 newlat) seen)))

'(f 2 '(3) (λ (newlat see)
            ((λ (newlat seen)
               (col (cons 1 newlat) seen))
             newlat
             (cons 2 seen))))

'(f 2 '() (λ (newlat seen)
           ((λ (newlat see)
              ((λ (newlat seen)
                 (col (cons 1 newlat) seen))
               newlat
               (cons 2 seen)))
            (cons 3 newlat) seen)))

(define col build)

((λ (newlat seen)
   ((λ (newlat see)
      ((λ (newlat seen)
         (col (cons 1 newlat) seen))
       newlat
       (cons 2 seen)))
    (cons 3 newlat) seen))
 '() '())

(define multirember
  (lambda (a lat)
    (f a
       lat
       (lambda (newlat seen) newlat))))

(multirember 2 '(1 2 3 1 2 3 4))

;; col stands for "collector".
;; A collector is sometimes called a "continuation"
