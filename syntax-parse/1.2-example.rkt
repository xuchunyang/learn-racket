#lang racket

;; ---------------------------------------------------------------
;; 1.2.1 Phases and Reusable Syntax Classes

#;
(module phase-mismatch-mod racket
  (require syntax/parse (for-syntax syntax/parse))
  (define-syntax-class foo
    (pattern (a b c)))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f:foo) #'(+ f.a f.b f.c)])))

#;
(module phase-ok-mod racket
  (require (for-syntax syntax/parse))
  (begin-for-syntax
    (define-syntax-class foo
      (pattern (a b c))))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f:foo) #'(+ f.a f.b f.c)])))

(module stxclass-mode racket
  (require syntax/parse)
  (define-syntax-class foo
    (pattern (a b c)))
  (provide foo))

#;
(module macro-mod racket
  (require (for-syntax syntax/parse 'stxclass-mode))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f.foo) #'(+ f.a f.b f.c)]))
  (provide macro))


;; ---------------------------------------------------------------
;; 1.2.2 Optional Keyword Arguments

(require (for-syntax syntax/parse))

(define-syntax (mycond stx)
  (syntax-parse stx
    [(mycond
      (~optional (~seq #:error-on-fallthrough who:expr))
      ;; (~or* (~seq #:error-on-fallthrough who:expr) (~seq))
      clause ...)
     (with-syntax ([error? (if (attribute who) #'#t #'#f)]
                   [who (or (attribute who) #'#f)])
       #'(mycond* error? who clause ...))]))

(define-syntax mycond*
  (syntax-rules ()
    [(mycond error? who [question answer] . clauses)
     (if question answer (mycond* error? who . clauses))]
    [(mycond #t who)
     (error who "no clauses matched")]
    [(mycond #f _)
     (void)]))

(mycond [(even? 13) 'blue]
        [(odd? 4) 'red])

#;
(mycond #:error-on-fallthrough 'myfun
        [(even? 13) 'blue]
        [(odd? 4) 'red])

(syntax-parse #'(a b 3)
  [(x:id ...) 'ok])


(syntax-parse #'(a b 3)
  [(x:id ...) 'ok])
