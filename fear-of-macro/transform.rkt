#lang racket

(define-syntax foo
  (lambda (stx)
    (syntax "I am foo")))

(foo)
;; => "I am foo"

(define-syntax (foo stx)
  (syntax "I am foo"))

(foo)
;; => "I am foo"

(define-syntax (foo stx)
  #'"I am foo")

(foo)
;; => "I am foo"

(define-syntax (say-hi stx)
  #'(displayln "hi"))

;; ---------------------------------------------------------------

(define-syntax (show-me stx)
  (print stx)
  #'(void))

(show-me '(+ 1 2))
;; => #<syntax::4714 (show-me (quote (+ 1 2)))>

(define stx #'(if x (list "true") #f))
stx
;; => #<syntax::4747 (if x (list "true") #f)>
(syntax-source stx)
;; => 'stdin
(syntax-line stx)
;; => #f
(syntax-column stx)
;; => #f
(syntax->datum stx)
;; => '(if x (list "true") #f)
(syntax-e stx)
#;
'(#<syntax::4748 if>
  #<syntax::4751 x>
  #<syntax::4753 (list "true")>
  #<syntax::4767 #f>)
(syntax->list stx)
#;
'(#<syntax::4748 if>
  #<syntax::4751 x>
  #<syntax::4753 (list "true")>
  #<syntax::4767 #f>)

(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(reverse-me "backwards" "am" "!" values)

(datum->syntax #f '(+ 1 2))
;; => #<syntax (+ 1 2)>

(if #f "true" "false")
;; => "false"

(syntax->list #'(if #f "true" "false"))
#;
'(#<syntax::6233 if>
  #<syntax::6236 #f>
  #<syntax::6239 "true">
  #<syntax::6246 "false">)

(require (for-syntax racket/list)) ; For compile time, second, third, fourth

(define-syntax (our-if stx)
  (define xs (syntax->list stx))
  (datum->syntax stx `(cond [,(second xs) ,(third xs)]
                            [else ,(fourth xs)])))

(our-if #f "ture" "false")
;; => "false"

(our-if #f
        (begin (displayln "true") "ture")
        (begin (displayln "false") "false"))
;; false
;; "false"

(require (for-syntax racket/match))     ; For compile time "match"
(define-syntax (our-if-v2 stx)
  (match (syntax->list stx)
    [(list name condition true-expr false-expr)
     (datum->syntax
      stx
      `(cond [,condition ,true-expr]
             [else ,false-expr]))]))

(our-if-v2
 #t
 (begin (displayln "true") "ture")
 (begin (displayln "false") "false"))
;; true
;; "ture"

;; ---------------------------------------------------------------
;; Review
;; ---------------------------------------------------------------

;; - define-syntax
;; - syntax #'
;; - datum->syntax, syntax->datum
;; - compile time vs run time
;; - require/for-syntax, begin-for-syntax
