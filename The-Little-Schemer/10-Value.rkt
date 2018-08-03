#lang racket

(require "lib.rkt")


;; Entry

(define new-entry build)

(define a-entry (new-entry '(one two three) '(1 2 3)))
a-entry
;; => '((one two three) (1 2 3))

(define lookup-in-entry
  (λ (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (λ (name names values entry-f)
    (cond [(null? names) (entry-f name)]
          [(eq? name (car names)) (car values)]
          [else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)])))

(lookup-in-entry 'one a-entry (λ (name) 'not-found))
;; => 1

(lookup-in-entry 'two a-entry (λ (name) 'not-found))
;; => 2

(lookup-in-entry 'three a-entry (λ (name) 'not-found))
;; => 3

(lookup-in-entry 'four a-entry (λ (name) 'not-found))
;; => 'not-found


;; Table (Environment)

;; A table is a list of entries.

(define extend-table cons)

(define lookup-in-table
  (λ (name table table-f)
    (cond [(null? table) (table-f name)]
          [else (lookup-in-entry
                 name
                 (car table)
                 (lambda (name)
                   (lookup-in-table name (cdr table) table-f)))])))

(define a-table '(((a b)
                   (1 2))
                  ((x y z)
                   (3 4 5))))


(lookup-in-table 'a a-table (λ (name) (format "~a is not found" name)))
;; => 1

(lookup-in-table 'b a-table (λ (name) (format "~a is not found" name)))
;; => 2

(lookup-in-table 'c a-table (λ (name) (format "~a is not found" name)))
;; => "c is not found"

(lookup-in-table 'x a-table (λ (name) (format "~a is not found" name)))
;; => 3

(lookup-in-table 'y a-table (λ (name) (format "~a is not found" name)))
;; => 4

(lookup-in-table 'z a-table (λ (name) (format "~a is not found" name)))
;; => 5



(cons 'car (cons (cons 'quote
                       (cons
                        (cons 'a
                              (cons 'b
                                    (cons 'c '())))
                        '()))
                 '()))
;; => '(car '(a b c))

(cons '(a b c) '())



;; Value

;; 6 -> *const
;; #f -> *const
;; car -> (primitive car)
;; quote -> *quote
;; nothing -> *identifier
;; (lambda (x y) (cons x y)) -> *lambda
;; ((lambda (x) x) #t) -> *application
;; cond -> *cond

(define expression-to-action
  (λ (e)
    (cond [(atom? e) (atom-to-action e)]
          [else (list-to-action e)])))

(define atom-to-action
  (λ (e)
    (cond [(number? e) *const]
          [(eq? e #t) *const]
          [(eq? e #f) *const]
          [(eq? e 'cons) *const]
          [(eq? e 'car) *const]
          [(eq? e 'cdr) *const]
          [(eq? e 'null?) *const]
          [(eq? e 'eq?) *const]
          [(eq? e 'atom?) *const]
          [(eq? e 'zero?) *const]
          [(eq? e 'add1) *const]
          [(eq? e 'sub1) *const]
          [(eq? e 'number?) *const]
          [else *identifier])))

(define list-to-action
  (λ (e)
    (cond
      [(atom? (car e))
       (cond [(eq? (car e) 'quote)
              *quote]
             [(eq? (car e) 'lambda)
              *lambda]
             [(eq? (car e) 'cond)
              *cond]
             [else
              *application])]
      [else *application])))



;; Interpreter

(define value
  (λ (e)
    (meaning e '())))

(define meaning
  (λ (e table)
    ((expression-to-action e) e table)))

(define *const
  (λ (e table)
    (cond [(number? e) e]
          [(eq? e #t) #t]
          [(eq? e #f) #f]
          [else (build (quote primitive) e)])))

(define *quote
  (λ (e table)
    (test-of e)))

(define test-of second)

(define *identifier
  (λ (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (λ (name)
    (car (quote ()))))

(define *lambda
  (λ (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(meaning '(lambda (x) (cons x y))
         '(((y z) ((8) 9))))

'(non-primitive
  ( (((y z) ((8) 9)))                   ; Table
    (x)                                 ; Formals
    (cons x y)                          ; Body
    ))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond [(else? (question-of (car lines)))
           (meaning (answer-of (car lines))
                    table)]
          [(meaning (question-of (car lines))
                    table)
           (meaning (answer-of (car lines))
                    table)]
          [else (evcon (cdr lines) table)])))

(define else?
  (λ (x)
    (cond [(atom? x) (eq? x 'else)]
          [else #f])))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(meaning '(cond (coffee klatsch)
                (else party))
         '(((coffee) (#t))
           ((klatsch pary) (5 (6)))))
;; => 5

(meaning 'meaning-of-everything '(((meaning-of-everything) (42))))
;; => 42

(define evlis
  (λ (args table)
    (cond [(null? args) '()]
          [else
           (cons (meaning (car args) table)
                 (evlis (cdr args) table))])))

(define *application
  (λ (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

;; Two kinds of functions:
;; - primitive => (primitive primitive-name)
;; - non-primitive => (non-primitive (table formals body)) => (table formals body) is called a closure record

(define primitive?
  (λ (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (λ (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (λ (fun vals)
    (cond [(primitive? fun)
           (apply-primitive
            (second fun) vals)]
          [(non-primitive? fun)
           (apply-closure
            (second fun) vals)])))

(define apply-primitive
  (λ (name vals)
    (cond [(eq? name 'cons)
           (cons (first vals) (second vals))]
          [(eq? name 'car)
           (car (first vals))]
          [(eq? name 'cdr)
           (cdr (first vals))]
          [(eq? name 'null?)
           (null? (first vals))]
          [(eq? name 'eq?)
           [eq? (first vals) (second vals)]]
          [(eq? name 'atom?)
           (:atom? (first vals))]
          [(eq? name 'zero?)
           (zero? (first vals))]
          [(eq? name 'add1)
           (add1 (first vals))]
          [(eq? name 'sub1)
           (sub1 (first vals))]
          [(eq? name 'number?)
           (number? (first vals))])))

(define :atom?
  (λ (x)
    (cond [(atom? x) #t]
          [(null? x) #f]
          [(eq? (car x) 'primitive?)
           #t]
          [(eq? (car x) 'non-primitive?)
           #t]
          [else #f])))

(define apply-closure
  (λ (closure vals)
    (meaning
     (body-of closure)
     (extend-table
      (new-entry (formals-of closure) vals)
      (table-of closure)))))

(meaning '(add1 41) '())
;; => 42

(meaning '(add1 x) '(((x) (41))))
;; => 42

(meaning '(sub1 (add1 x)) '(((x) (42))))
;; => 42

(meaning '((lambda (x y) (cons x (cons y (cons z '())))) 1 2)
         '(((x y z) (0 0 3))))
;; => '(1 2 3)
