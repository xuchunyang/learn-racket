#lang racket

(provide check-ids)

(define (check-ids stx forms)
  (for ([form (in-list (syntax->list forms))])
    (unless (identifier? form)
      (raise-syntax-error #f
                          "not an identifier"
                          stx
                          form))))
