#lang racket

;; ---------------------------------------------------------------
;; Robust Function

(define/contract (misuse s)
  (string? . -> . string?)
  (string-append s " snazzy suffix"))

;; (misuse 123)

(module a typed/racket
  (: misuse (String -> String))
  (define (misuse s)
    (string-append s " snazzy suffix"))
  ;; (misuse 0)
  )


;; ---------------------------------------------------------------
;; syntax-parse
