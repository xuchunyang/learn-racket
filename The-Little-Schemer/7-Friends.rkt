#lang racket

(require rackunit)

(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(define member?
  (λ (a lat)
    (cond [(null? lat) #f]
          [else (or (eq? a (car lat))
                    (member? a (cdr lat)))])))

(define multirember
  (λ (a lat)
    (cond
      [(null? lat) '()]
      [else
       (cond [(eq? a (car lat))
              (multirember a (cdr lat))]
             [else
              (cons (car lat) (multirember a (cdr lat)))])])))

(module+ test
  (check-true (member? 'b '(a b c)))
  (check-false (member? 'd '(a b c)))
  (check-false (member? 'a '())))

(define set?
  (λ (lat)
    (cond [(null? lat) #t]
          [(member? (car lat) (cdr lat)) #f]
          [else (set? (cdr lat))])))

(module+ test
  (check-false (set? '(apple peaches apple plum)))
  (check-true (set? '())))



'(apple peach pear peach plum apple lemon peach)

(define makeset
  (λ (lat)
    (cond [(null? lat) '()]
          [(member? (car lat) (cdr lat))
           (makeset (cdr lat))]
          [else (cons (car lat) (makeset (cdr lat)))])))

(makeset '(1 2 3 3 2 1))
;; => '(3 2 1)

(define makeset2
  (λ (lat)
    (cond [(null? lat) '()]
          [else (cons (car lat)
                      (makeset2
                       (multirember (car lat)
                                    (cdr lat))))])))

(makeset2 '(1 2 3 3 2 1))
;; => '(1 2 3)

(makeset2 '(apple 3 pear 4 9 apple 3 4))
;; => '(apple 3 pear 4 9)



(define subset?
  (λ (set1 set2)
    (cond [(null? set1) #t]
          [else
           (and
            (member? (car set1) set2)
            (subset? (cdr set1) set2))])))

(subset? '(5 chicken wings)
         '(5 hamburgers
             2 pieces fried chicken and
             light duckling wings))
;; => #t



(define eqset?
  (λ (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? '() '())
;; => #t

(eqset? '(1 2 3) '(3 2 1))
;; => #t



;; Return #t if at least one atom in set1 is in set2
(define intersect?
  (λ (set1 set2)
    (cond [(null? set1) #f]
          [(member? (car set1) set2) #t]
          [else (intersect? (cdr set1) set2)])))




(define intersect
  (λ (set1 set2)
    (cond [(null? set1) '()]
          [(member? (car set1) set2)
           (cons (car set1) (intersect (cdr set1) set2))]
          [else
           (intersect (cdr set1) set2)])))

(intersect '(1 2 3) '(2 3 4))
;; => '(2 3)



(define union
  (λ (set1 set2)
    (cond [(null? set1) set2]
          [(member? (car set1) set2)
           (union (cdr set1) set2)]
          [else
           (cons (car set1)
                 (union (cdr set1) set2))])))

(union '(1 2 3) '(2 3 4))
;; => '(1 2 3 4)

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))
;; => '(stewed tomatoes casserole macaroni and cheese)



(define difference
  (λ (set1 set2)
    (cond [(null? set1) '()]
          [(member (car set1) set2)
           (difference (cdr set1) set2)]
          [else (cons (car set1)
                      (difference (cdr set1) set2))])))

(difference '(1 2 3 4) '(2 3))
;; => '(1 4)



(define intersectall
  (λ (l-set)
    (cond [(null? (cdr l-set)) (car l-set)]
          [else
           (intersectall
            (cons (intersect (car l-set)
                             (car (cdr l-set)))
                  (cdr (cdr l-set))))])))

(intersectall '((1)))
;; => '(1)

(intersectall '((1 3) (1 2 3 4) (1)))
;; => '(1)

(define intersectall2
  (λ (l-set)
    (cond [(null? (cdr l-set)) (car l-set)]
          [else (intersect (car l-set)
                           (intersectall2 (cdr l-set)))])))
(intersectall2
 '((6 pears and)
   (3 peaches and 6 peppers)
   (8 pears and 6 plums)
   (and 6 prunes with some apples)))
;; => '(6 and)

;; Pair is a list with only two S-expression

(define a-pair?
  (λ (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [(null? (cdr (cdr x))) #t]
      [else #f])))

(a-pair? '(3 7))
;; => #t



(define first (λ (p) (car p)))
(define second (λ (p) (car (cdr p))))
(define third (λ (p) (car (cdr (cdr p)))))
(define build (λ (s1 s2) (cons s1 (cons s2 (quote ())))))



;; rel -> relation (二元关系)
;; 
;; A relation is a list of paris

;; function is a non-repeated relation

(define firsts
  (λ (lists)
    (cond [(null? lists) '()]
          [else (cons (car (car lists))
                      (firsts (cdr lists)))])))
(define fun?
  (λ (rel)
    (set? (firsts rel))))

(fun? '((4 3)
        (4 2)
        (7 6)
        (6 2)
        (3 4)))
;; => #f

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
;; => #t

(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
;; => #f



(define revrel
  (λ (rel)
    (cond [(null? rel) '()]
          [else (cons (revpair (car rel))
                      (revrel (cdr rel)))])))

(define revpair
  (λ (pair)
    (build (second pair) (first pair))))

(revrel '((8 a)
          (pumpkin pie)
          (got sick)))
;; => '((a 8) (pie pumpkin) (sick got))



(define fullfun?
  (λ (fun)
    (fun? (revrel fun))))

(fullfun? '((grape raisin)
            (plum prune)
            (stewed prune)))
;; => #f

(fullfun? '((8 3)
            (4 8)
            (7 6)
            (6 2)
            (3 4)))
;; => #t



(define seconds
  (λ (lists)
    (cond [(null? lists) '()]
          [else (cons (second (car lists))
                      (seconds (cdr lists)))])))

(seconds '((a 1)
           (b 2)
           (c 3)))
;; => '(1 2 3)

(define one-to-one?
  (λ (fun)
    (fun? (revrel fun))))
