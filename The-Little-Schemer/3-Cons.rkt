#lang racket

(define rember
  (λ (elt lst)
    (cond [(null? lst) '()]
          [(eq? elt (car lst)) (cdr lst)]
          [else (cons (car lst) (rember elt (cdr lst)))])))

(require rackunit)
(check-equal? '(a c) (rember 'b '(a b c)))
(check-equal? '(b c a) (rember 'a '(a b c a)))
(check-equal? '(a b c a) (rember 'x '(a b c a)))
(check-equal? '(x x x) (rember 'x '(x x x x)))
(check-equal? '() (rember 'x '()))

(rember 'mint '(lamb chops and mint jelly))
;; => '(lamb chops and jelly)

(rember 'mint '(lamb chops and mint flavored mint jelly))
;; => '(lamb chops and flavored mint jelly)

(rember 'toast '(bacon lettuce and tomato))
;; => '(bacon lettuce and tomato)

(rember 'cup '(coffee cup tea cup and hick up))
;; => '(coffee tea cup and hick up)

;; (rember a lat)
;;
;; It takes an atom and a lat as its arguments, and makes a new lat
;; with the first occurrence of the atom in the old lat removed.

(rember 'sauce '(soy sauce and tomato sauce))
;; => '(soy and tomato sauce)

(define firsts
  (λ (lists)
    (cond [(null? lists) '()]
          [else (cons (car (car lists))
                      (firsts (cdr lists)))])))

(check-equal? '(1 4 7)
              (firsts '((1 2 3)
                        (4 5 6)
                        (7 8 9))))

(check-equal? '() (firsts '()))

(firsts '((a b)
          (c d)
          (e f)))
;; => '(a c e)

(firsts '())
;; => '()

(firsts '((five plums)
          (four)
          (eleven green oranges)))
;; => '(five four eleven)

(firsts '(((five plums) four)
          (eleven green oranges)
          ((no) more)))
;; => '((five plums) eleven (no))


;; (firsts l)
;;
;; The function firsts takes one argument, a list, which is either a
;; null list or contains non-empty lists. It builds another list
;; composed of the first S-expression of each internal list.

;; (insertR new old lat)

(define insertR
  (λ (new old lat)
    (cond [(null? lat) '()]
          [(eq? old (car lat))
           (cons old (cons new (insertR new old (cdr lat))))]
          [else (cons (car lat) (insertR new old (cdr lat)))])))

(check-equal? (insertR 'A 'a '(a b c a)) '(a A b c a A))
(check-equal? (insertR 'A 'a '(b c d e)) '(b c d e))
(check-equal? (insertR 'A 'a '()) '())

(insertR 'topping 'fudge '(ice cream with fudge for dessert))

(define insertL
  (λ (new old lat)
    (cond [(null? lat) '()]
          [else (cond [(eq? old (car lat))
                       (cons new lat)]
                      [else
                       (cons (car lat) (insertL new old (cdr lat)))])])))

(check-equal? (insertL 'found 'a '(a b c a))
              '(found a b c a))

(define subst
  (λ (new old lat)
    (cond [(null? lat) '()]
          [else (cond [(eq? old (car lat)) (cons new (cdr lat))]
                      [else (cons (car lat) (subst new old (cdr lat)))])])))

(check-equal? '(A b c a)
              (subst 'A 'a '(a b c a)))

(check-equal? '() (subst 'A 'a '()))

(subst 'topping 'fudge '(ice cream with fudge for dessert))
;; => '(ice cream with topping for dessert)

(define subst2
  (λ (new o1 o2 lat)
    (cond [(null? lat) '()]
          [else (cond [(or (eq? o1 (car lat))
                           (eq? o2 (car lat)))
                       (cons new (cdr lat))]
                      [else
                       (cons (car lat) (subst2 new o1 o2 (cdr lat)))])])))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
;; => '(vanilla ice cream with chocolate topping)

(define multirember
  (λ (a lat)
    (cond
      [(null? lat) '()]
      [else
       (cond [(eq? a (car lat))
              (multirember a (cdr lat))]
             [else
              (cons (car lat) (multirember a (cdr lat)))])])))

(multirember 'a '(a b c a))
;; => '(b c)

(define multiinsertR
  (λ (new old lat)
    (cond [(null? lat) '()]
          [else
           (cond
             [(eq? old (car lat))
              (cons old (cons new (multiinsertR new old (cdr lat))))]
             [else
              (cons (car lat) (multiinsertR new old (cdr lat)))])])))

(multiinsertR 'A 'a '(a b c a))
;; => '(a A b c a A)

(define multiinsertL
  (λ (new old lat)
    (cond [(null? lat) '()]
          [else
           (cond [(eq? old (car lat))
                  (cons new (cons old (multiinsertL new old (cdr lat))))]
                 [else
                  (cons (car lat) (multiinsertL new old (cdr lat)))])])))

(multiinsertL 'A 'a '(1 a b a 2))
;; => '(1 A a b A a 2)

(define multisubst
  (λ (new old lat)
    (cond [(null? lat) '()]
          [else (cond [(eq? old (car lat))
                       (cons new (multisubst new old (cdr lat)))]
                      [else
                       (cons (car lat) (multisubst new old (cdr lat)))])])))

(multisubst 'A 'a '(1 a b c a 2))
;; => '(1 A b c A 2)
