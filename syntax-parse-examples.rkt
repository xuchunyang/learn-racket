;; https://docs.racket-lang.org/syntax-parse-example/index.html ;;
#lang racket

;; ---------------------------------------------------------------

(require (for-syntax racket/list))
(define-syntax (K args-stx)
  (define args (syntax-e args-stx))
  (if (= (length args) 3)
      (second args)
      (raise-argument-error
       'K
       "syntax object containing a list with 3 elements"
       args-stx)))

(K 1 2)
;; => 1

;; (K 1) ;;

(require (for-syntax racket/base syntax/parse))
(define-syntax (K2 args-stx)
  (syntax-parse args-stx
    [(_ ?arg0 ?arg1)
     #'?arg0]))

(K2 1 2)
;; (K2 1) ;;

;; ---------------------------------------------------------------
;; First-class-or

(or #false #true)
;; => #t

(define-syntax (first-class-or stx)
  (syntax-parse stx
    [(_)
     #'#false]
    [(_ ?a . ?b)
     #'(let ([a-val ?a])
         (if a-val a-val (first-class-or . ?b)))]
    [_:id
     #'(lambda arg*
         (let loop ([arg* arg*])
           (cond
             [(null? arg*)
              (displayln "hello")
              #f]
             [(car arg*)
              (car arg*)]
             [else
              (loop (cdr arg*))])))]))

(first-class-or #f #t 0)
(apply first-class-or '())
(apply first-class-or '(#f #t 0))
(map first-class-or '(9 #f 3) '(8 #f #t))
