;; https://docs.racket-lang.org/guide/pattern-macros.html
#lang racket

;; -------------------------------------------------------------------------

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(let ([a 1] [b 2])
  (swap a b)
  (list a b))
;; => '(2 1)

;; 同名不会使 Racket 困惑
(let ([tmp 1]
      [x 2])
  (swap tmp x)
  (list tmp x))
;; => '(2 1)

;; -------------------------------------------------------------------------

;; 函数无法解决
(define (incf-not-working var)
  (set! var (add1 var)))

(let ((x 1))
  (incf-not-working x)
  x)
;; => 1

(define-syntax-rule (incf var)
  (set! var (add1 var)))

(let ((x 1))
  (incf x)
  x)
;; => 2

;; -------------------------------------------------------------------------

;; 比 swap 多支持一个三个参数的情况
(define-syntax rotate
  (syntax-rules ()
    [(rotate a b) (swap a b)]
    [(rotate a b c) (begin (swap a b)
                           (swap b c))]))

(let-values ([(a b c) (values 1 2 3)])
  (rotate a b c)
  (list a b c))
;; => '(2 3 1)

;; 支持任意多的参数
(define-syntax rotate2
  (syntax-rules ()
    [(rotate2 a) (void)]
    ;; c ... 匹配 0 或更多位置
    [(rotate2 a b c ...) (begin
                           (swap a b)
                           (rotate2 b c ...))]))

(let-values ([(a b c d) (values 1 2 3 4)])
  (rotate2 a b c d)
  (list a b c d))
;; => '(2 3 4 1)

;; 更高效的 rotate
(define-syntax rotate3
  (syntax-rules ()
    [(rotate3 a c ...)
     (shift-to (c ... a) (a c ...))]))

(define-syntax shift-to
  (syntax-rules ()
    [(shift-to (from0 from ...) (to0 to ...))
     (let ([tmp from0])
       (set! to from) ...
       (set! to0 tmp))]))

(let-values ([(a b c d) (values 1 2 3 4)])
  (rotate3 a b c d)
  (list a b c d))
;; => '(2 3 4 1)

;; ---------------------------------------------------------------

(define-syntax val
  (lambda (stx)
    (syntax-case stx ()
      [val (identifier? (syntax val)) (syntax (get-val))])))

(define-values (get-val put-val!)
  (let ([private-val 0])
    (values (lambda () private-val)
            (lambda (v) (set! private-val v)))))

val
;; => 0

(+ val 3)
;; => 3

;; (val)

(define-syntax val2
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [val2 (identifier? (syntax val2)) (syntax (get-val))]
       [(set! val2 e) (syntax (put-val! e))]))))

(define-syntax val2
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [val2 (identifier? (syntax val2)) (syntax (get-val))]
       [(set! val2 e) (syntax (put-val! e))]))))

val2
;; => 0

(+ val2 3)
;; => 3

(set! val2 10)
val2
;; => 10

;; ---------------------------------------------------------------

;; 生成 Macro 的 macro
(define-syntax-rule (define-get/put-id id get put!)
  (define-syntax id
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [id (identifier? (syntax id)) (syntax (get))]
         [(set! id e) (syntax (put! e))])))))

(define-get/put-id val3 get-val put-val!)

val3
;; => 10

(set! val3 11)
val3
;; => 11

;; ---------------------------------------------------------------

(define-syntax-rule (define-cbr (id arg ...) body)
  (begin
    (define-syntax id
      (syntax-rules ()
        [(id actual (... ...))
         (do-f (lambda () actual)
               (... ...)
               (lambda (v)
                 (set! actual v))
               (... ...))]))
    (define-for-cbr do-f (arg ...)
      ()
      body)))

(define-syntax define-for-cbr
  (syntax-rules ()
    [(define-for-cbr do-f (id0 id ...)
       (gens ...) body)
     (define-for-cbr do-f (id ...)
       (gens ... (id0 get put)) body)]
    [(define-for-cbr do-f ()
       ((id get put) ...) body)
     (define (do-f get ... put ...)
       (define-get/put-id id get put) ...
       body)]))

(define-cbr (f a b)
  (swap a b))
 
(let ([x 1] [y 2])
  (f x y)
  (list x y))
