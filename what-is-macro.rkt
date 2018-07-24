;; https://docs.racket-lang.org/guide/stx-obj.html

#lang racket

;; ---------------------------------------------------------------
;; Syntax Object

(syntax (+ 1 2))
;; => '(syntax:stdin:: (+ 1 2))

#'(+ 1 2)
;; => '(syntax:stdin:: (+ 1 2))

(identifier? #'car)
;; => #t

(syntax-e #'car)
;; => 'car

(free-identifier=? #'car #'cdr)
;; => #f

(free-identifier=? #'car #'car)
;; => #t

(require (only-in racket/base [car also-car]))

(free-identifier=? #'car #'also-car)
;; => #t

#'(+ 1 2)
;; => #<syntax::310 (+ 1 2)>

(syntax-e #'(+ 1 2))
;; => '(#<syntax::352 +> #<syntax::354 1> #<syntax::356 2>)

(syntax->datum #'(+ 1 2))
;; => '(+ 1 2)

(datum->syntax #'lex
               '(+ 1 2)
               #'srcloc)
;; => #<syntax::448 (+ 1 2)>

;; ---------------------------------------------------------------

;; - Macro 的作用是把一个 Syntax Object 转换成另一个 Syntax Object
;; - 用 define-syntax 定义一个 Macro
;; - Macro 是一个函数， 接收一个 Syntax Object，返回一个 Syntax Object

;; 任何一参的函数都可以是 Macro Transformer
(syntax-rules () [(nothing) something])
;; => #<procedure>

(define-syntax self-as-string
  (lambda (stx)
    (datum->syntax stx (format "~s" (syntax->datum stx)))))

(self-as-string (+ 1 2))
;; => "(self-as-string (+ 1 2))"

(define-syntax (self-as-string2 stx)
  (datum->syntax stx (format "~s" (syntax->datum stx))))

(self-as-string2 (+ 1 2))
;; => "(self-as-string2 (+ 1 2))"

;; ---------------------------------------------------------------

;; Pattern matching on syntax object
(syntax->datum
 (syntax-case #'(+ 1 2) ()
   [(op n1 n2) #'(- n1 n2)]))
;; => '(- 1 2)

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define-syntax swap2
  (syntax-rules ()
    [(swap2 x y) (let ([tmp x])
                   (set! x y)
                   (set! y tmp))]))

(define-syntax (swap3 stx)
  (syntax-case stx ()
    [(swap3 x y) #'(let ([tmp x])
                     (set! x y)
                     (set! y tmp))]))

(let ([a 1] [b 2])
  (swap a b)
  (list a b))
;; => '(2 1)

(define-syntax (swap4 stx)
  (syntax-case stx ()
    [(swap4 x y)
     (if (and (identifier? #'x)
              (identifier? #'y))
         #'(let ([tmp x])
             (set! x y)
             (set! y tmp))
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             (if (identifier? #'x)
                                 #'y
                                 #'x)))]))

;; (swap4 x 3)
; stdin::5147: swap4: not an identifier
;   at: 3
;   in: (swap4 x 3)
;; (with-syntax)


;; ---------------------------------------------------------------

(define-syntax (incf-by-random stx)
  (syntax-case stx ()
    [(incf-by-random i)
     (with-syntax ([rand
                    100
                    ;; (random 100)
                    ])
       #'(set! i (+ rand i)))]))

(let ([x 1])
  (incf-by-random x)
  x)
;; => 9

(generate-temporaries '(a b c d))
;; => '(#<syntax a1> #<syntax b2> #<syntax c3> #<syntax d4>)

(generate-temporaries '(1 2 3 4))
;; => '(#<syntax temp5> #<syntax temp6> #<syntax temp7> #<syntax temp8>)

(define-syntax (set!-values stx)
  (syntax-case stx ()
    [(set!-values (id ...) expr)
     (with-syntax ([(temp ...) (generate-temporaries #'(id ...))])
       #'(let-values ([(temp ...) expr])
           (set! id temp) ... (void)))]))

(let ([a 1] [b 2] [c 3])
  (set!-values (a b c) (values 100 200 300))
  (list a b c))
;; => '(100 200 300)

(define-syntax (set!-values2 stx)
  (syntax-case stx ()
    [(set!-values (id ...) expr)
     #'(let-values ([(id ...) expr])
         (set! id id) ... (void))]))

(let ([a 1] [b 2] [c 3])
  (set!-values2 (a b c) (values 100 200 300))
  (list a b c))
;; => '(1 2 3)

;; ---------------------------------------------------------------

;; Compile and Run-Time Phases
(define-syntax (swap5 stx)
  (syntax-case stx ()
    [(swap x y) (begin
                  (displayln "Hello World")
                  (check-ids stx #'(x y))
                  #'(let ([tmp x])
                      (set! x y)
                      (set! y tmp)))]))

(begin-for-syntax
  (define (check-ids stx forms)
    (for ([form (syntax->list forms)])
      (unless (identifier? form)
        (raise-syntax-error #f
                            "not an identifier"
                            stx
                            form)))))

(let ([a 1] [b 2]) (swap5 a b))
