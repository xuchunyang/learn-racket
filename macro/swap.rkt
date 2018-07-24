#lang racket

(require (for-syntax "utils.rkt"))

(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y) (begin
                  (check-ids stx #'(x y))
                  #'(let ([tmp x])
                      (set! x y)
                      (set! y tmp)))]))

;; 编译期间报错
;; (swap 1 2)
;; 同样是在编译期间报错
;; (swap x y)

;; 没有问题
#;
(let ([a 1] [b 2])
  (sleep 5)
  (swap a b))
