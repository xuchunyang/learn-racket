#lang racket

(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l)) (lat? (cdr l))]
        (else #f)))

(define rember*
  (λ (a l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? a (car l))
                  (rember* a (cdr l))]
                 [else (cons (car l)
                             (rember* a (cdr l)))])]
          [else (cons (rember* a (car l))
                      (rember* a (cdr l)))])))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
;; => '((coffee) ((tea)) (and (hick)))


(rember* 'sauce '(((tomato sauce))
                  ((bean) sauce)
                  (and ((flying)) sauce)))
;; => '(((tomato)) ((bean)) (and ((flying))))



(define insertR*
  (λ (new old l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? old (car l))
                  (cons old
                        (cons new
                              (insertR* new old (cdr l))))]
                 [else (cons (car l)
                             (insertR* new old (cdr l)))])]
          [else (cons (insertR* new old (car l))
                      (insertR* new old (cdr l)))])))

(insertR* 'roast
          'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))

#;
'((how much (wood))
  could
  ((a (wood) chuck roast))
  (((chuck roast)))
  (if (a) ((wood chuck roast)))
  could
  chuck
  roast
  wood)



(define occur*
  (λ (a l)
    (cond [(null? l) 0]
          [(atom? (car l))
           (cond [(eq? a (car l))
                  (add1 (occur* a (cdr l)))]
                 [else (occur* a (cdr l))])]
          [else (+ (occur* a (car l))
                   (occur* a (cdr l)))])))

(occur* 'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))
;; => 5



(define subst*
  (λ (new old l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? old (car l))
                  (cons new (subst* new old (cdr l)))]
                 [else
                  (cons (car l) (subst* new old (cdr l)))])]
          [else
           (cons (subst* new old (car l))
                 (subst* new old (cdr l)))])))

(subst* 'orange
        'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))

#;
'((orange)
  (split ((((orange ice))) (cream (orange)) sherbet))
  (orange)
  (bread)
  (orange brandy))



(define insertL*
  (λ (new old l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? old (car l))
                  (cons new
                        (cons old
                              (insertL* new old (cdr l))))]
                 [else (cons (car l)
                             (insertL* new old (cdr l)))])]
          [else (cons (insertL* new old (car l))
                      (insertL* new old (cdr l)))])))

(insertL* 'pecker
          'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))

#;
'((how much (wood))
  could
  ((a (wood) pecker chuck))
  (((pecker chuck)))
  (if (a) ((wood pecker chuck)))
  could
  pecker
  chuck
  wood)



(define member*
  (λ (a l)
    (cond [(null? l) #f]
          [(atom? (car l))
           (cond [(eq? a (car l)) #t]
                 [else (member* a (cdr l))])]
          [else (or (member* a (car l))
                    (member* a (cdr l)))])))

(member* 'chips '((potato) (chips ((with) fish) (chips))))
;; => #t

(member* 'foo '((potato) (chips ((with) fish) (chips))))
;; => #f



(define leftmost
  (λ (l)
    (cond [(atom? (car l)) (car l)]
          [else (leftmost (car l))])))

(leftmost '((potato) (chips ((with) fish) (chips))))
;; => 'potato

(leftmost '(((hot) (tuna (and))) cheese))
;; => 'hot



(define eqlist?
  (λ (l1 l2)
    (cond [(and (null? l1) (null? l2)) #t]
          [(or (null? l1) (null? l2)) #f]
          [else (and (equal? (car l1) (car l2))
                     (equal? (cdr l1) (cdr l2)))])))

(eqlist? '(strawberry ice cream)
         '(strawberry ice cream))
;; => #t

(eqlist? '(strawberry ice cream)
         '(strawberry cream ice))
;; => #f

(eqlist? '(banana ((split)))
         '((banana) (split)))
;; => #f

(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((salami)) (and (soda))))
;; => #f

(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((sausage)) (and (soda))))
;; => #t


;; What is an S-expression?
;;
;; An S-expression is either an atom or a (possibly empty) list of
;; S-expressions.

#;
(define equal?
  (λ (s1 s2)
    (cond [(and (atom? s1)
                (atom? s2))
           (eq? s1 s2)]
          [(or (atom? s1)
               (atom? s2))
           #f]
          [else (eqlist? s1 s2)])))
