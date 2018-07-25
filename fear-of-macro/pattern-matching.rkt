#lang racket

(require (for-syntax racket/match))
(define-syntax (our-if-v2 stx)
  (match (syntax->list stx)
    [(list name condition true-expr false-expr)
     (datum->syntax
      stx
      `(cond [,condition ,true-expr]
             [else ,false-expr]))]))

(module+ test
  (require rackunit)
  (check-eqv? (our-if-v2 #f 1 2) 2)
  (check-eqv? (our-if-v2 #t 1 2) 1))

(define-syntax (our-if-using-syntax-case stx)
  (syntax-case stx ()
    [(name condition true-expr false-expr)
     #'(cond [condition true-expr]
             (else false-expr))]))

(module+ test
  (check-eqv? (our-if-using-syntax-case #f 1 2) 2)
  (check-eqv? (our-if-using-syntax-case #t 1 2) 1))

(define-syntax-rule (our-if-using-syntax-rule condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))

(module+ test
  (check-eqv? (our-if-using-syntax-rule #f 1 2) 2)
  (check-eqv? (our-if-using-syntax-rule #t 1 2) 1))

;; ---------------------------------------------------------------

;; (hyphen-define a b (args) body)
;; (define (a-b args) body)

(define-syntax (hyphen-define stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax
                   #'a
                   (string->symbol (format "~a-~a"
                                           (syntax->datum #'a)
                                           (syntax->datum #'b)))) ()
       [name #'(define (name args ...)
                 body0 body ...)])]))

(hyphen-define foo bar () (define x 1) x)
(foo-bar)
;; => 1

;; ---------------------------------------------------------------

(define-syntax (hyphen-define/ok2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (datum->syntax #'a
                                        (string->symbol (format "~a-~a"
                                                                (syntax->datum #'a)
                                                                (syntax->datum #'b))))])
       #'(define (name args ...)
           body0 body ...))]))

(hyphen-define foo bar () (define x 1) x)
(foo-bar)
;; => 1

(require (for-syntax racket/syntax))
(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax* ([b #'a]
                    [c #'b])
       #'c)]))

(foo (+ 1 2 3))

(require (for-syntax racket/syntax))
(require racket/syntax)
(define-syntax (hyphen-define/ok3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (format-id #'a "~a-~a" #'a #'b)])
       #'(define (name args ...)
           body0 body ...))]))

(format-id #f "hello")
;; => #<syntax hello>

(format-id #f "~a-~a" #'a #'b)
;; => #<syntax a-b>

(datum->syntax
 #f
 (string->symbol
  (format "~a-~a"
          (syntax->datum #'a)
          (syntax->datum #'b))))
;; => #<syntax a-b>

(hyphen-define/ok3 foo bar () #t)
(foo-bar)
;; => #t

;; ---------------------------------------------------------------

(hyphen-define* (foo bar baz) (v) (* 2 v))

(require (for-syntax racket/string racket/syntax))

(define-syntax (hyphen-define* stx)
  (syntax-case stx ()
    [(_ (names ...) (args ...) body0 body ...)
     (let ([name-stxs (syntax->list #'(names ...))])
       (with-syntax ([name
                      (datum->syntax
                       (car name-stxs)
                       (string->symbol
                        (string-join (for/list ([name-stx name-stxs])
                                       (symbol->string
                                        (syntax-e name-stx)))
                                     "-")))])
         #'(define (name args ...)
             body0 body ...)))]))

(define-syntax (hyphen-define* stx)
  (syntax-case stx ()
    [(_ (name0 names ...) (args ...) body0 body ...)
     (let ([name-stxs (syntax->list #'(name0 names ...))])
       (with-syntax ([name
                      (datum->syntax
                       #'name0
                       (string->symbol
                        (string-join (for/list ([name-stx name-stxs])
                                       (symbol->string
                                        (syntax-e name-stx)))
                                     "-")))])
         #'(define (name args ...)
             body0 body ...)))]))

(hyphen-define* (foo bar baz) (v) (* 2 v))
(foo-bar-baz 3)

;; ---------------------------------------------------------------

(our-struct name (field1 field2 ...))
(require (for-syntax racket/syntax))
(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     ;; Guard expression
     (for-each (lambda (x)
                 (unless (identifier? x)
                   (raise-syntax-error #f "not an identifier" stx x)))
               (cons #'id (syntax->list #'(fields ...))))
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
       #`(begin
           ;; Define a constructor.
           (define (id fields ...)
             (apply vector (cons 'id (list fields ...))))
           ;; Define a predicate
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #'(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))


(foo 1 2)
;; => '#(foo 1 2)
(require rackunit)
(our-struct foo (a b))
(define s (foo 1 2))
(check-true (foo? s))
(check-false (foo? 1))
(check-true (foo? #(foo 100 200)))
(check-equal? (foo-a s) 1)
(check-equal? (foo-b s) 2)
(check-exn exn:fail?
           (lambda () (foo-a "furble")))

;; (our-struct "foo" ("bar" "baz"))
(define ht (hasheq 'a (hasheq 'b (hasheq 'c 42))))

(hash-ref (hash-ref (hash-ref ht 'a) 'b) 'c)
;; => 42

(define (hash-refs h ks)
  (for/fold ([x h])
            ([k ks])
    (hash-ref x k)))

(hash-refs ht '(a b c))
;; => 42

;; (hash.refs ht.a.b.c)
(define-syntax (hash.refs stx)
  (syntax-case stx ()
    [(_ chain)
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (string-split chain-str ".")])
                   (format-id #'chain "~a" str))])
       (with-syntax ([hash-table (car ids)]
                     (keys       (cdr ids)))
         #'(hash-refs hash-table 'keys)))]))

(hash.refs ht.a.b.c)
;; => 42

(hash.refs ht.blah)
