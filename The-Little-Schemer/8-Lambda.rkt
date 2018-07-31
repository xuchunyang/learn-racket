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
