#lang racket/base

(display "What's your name? ")
(define name (read-line))
(printf "Hello, ~a!\n" (string-titlecase name))
