#lang racket

;; ---------------------------------------------------------------
;; Introduction

(define-syntax-rule (mylet ([var rhs] ...) body ...)
  ((lambda (var ...) body ...) rhs ...))

(mylet ([x 1] [y 2])
       (+ x y))

(mylet [x 1]
       x)

(mylet ([1 x])
       x)

(require (for-syntax syntax/parse))

(define-syntax (mylet stx)
  (syntax-parse stx
    [(_ ([var:id rhs:expr] ...) body ...+)
     #'((lambda (var ...) body ...) rhs ...)]))

(mylet (["a" 1])
       (add1 a))

(mylet ([a #:whoops])
       1)

(mylet ([a 1 2]) (* a a))

(define-syntax (mylet stx)
  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (syntax-parse stx
    [(_ (b:binding ...) body ...+)
     #'((lambda (b.var ...) body ...) b.rhs ...)]))

(mylet [a 1] (* a a))
(mylet (["a" 1]) (* a a))

(mylet ([a 1] [a 2]) a)
;; (let ([a 1] [a 2]) a)

(define-syntax (mylet stx)
  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (syntax-parse stx
    [(_ (b:binding ...) body ...+)
     #:fail-when
     (check-duplicate-identifier
      (syntax->list #'(b.var ...)))
     "duplicate variable name !!!"
     #'((lambda (b.var ...) body ...) b.rhs ...)]))

(mylet ([a 1] [a 2]) a)

(define-syntax (mylet stx)

  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (define-syntax-class distinct-bindings
    #:description "sequence of distinct binding pair"
    (pattern (b:binding ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(b.var ...)))
             "duplicate variable name"
             #:with (var ...) #'(b.var ...)
             #:with (rhs ...) #'(b.rhs ...)))

  (syntax-parse stx
    [(_ bs:distinct-bindings . body)
     #'((lambda (bs.var ...) . body) bs.rhs ...)]))

(mylet ([a 1] [b 2]) (+ a b))
(mylet ([a 1] [a 2]) a)

(define-syntax (mylet stx)

  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (define-syntax-class distinct-bindings
    #:description "sequence of distinct binding pair"
    (pattern (b:binding ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(b.var ...)))
             "duplicate variable name"
             #:with (var ...) #'(b.var ...)
             #:with (rhs ...) #'(b.rhs ...)))

  (syntax-parse stx
    [(_ bs:distinct-bindings . body)
     #'((lambda (bs.var ...) . body) bs.rhs ...)]
    [(_ loop:id bs:distinct-bindings . body)
     #'(letrec ([loop (lambda (bs.var ...) . body)])
         (loop bs.rhs ...))]))

(mylet loop
       ([i 3])
       (when (> i 0)
         (displayln i)
         (loop (- i 1))))

(mylet)
