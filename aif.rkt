#lang racket/base

(provide it aif)

(require racket/stxparam)

(define-syntax-parameter it (syntax-rules ()))

(define-syntax aif
  (syntax-rules ()
    [(aif test then else)
     (let ([t test])
       (syntax-parameterize ([it (syntax-id-rules () [_ t])])
         (if t then else)))]))
