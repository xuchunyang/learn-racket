#lang racket

;; The First Commandment
;;
;; (first revision)
;;
;; When recurring on a list of atoms, lat, ask two questions about it:
;; (null? lat) and else.
;; When recurring on a number, n, ask two questions about it:
;; (zero? n) and else.

;; The Second Commandment
;;
;; Use cons to build list

;; The Third Commandment
;;
;; When building a list, describe the first typical element, and then
;; cons it onto the natural recursion.

;; The Fourth Commandment
;;
;; (first revision)
;; 
;; Always change at least one argument while recurring. It must be
;; changed to be closer to the termination condition:
;; when using cdr, test termination with null? and
;; when using sub1, test termination with zero?

;; The Fifth Commandment
;;
;; When building a value with +, always use 0 for the value of the
;; terminating line, for adding 0 does not change the value of an
;; addition.
;;
;; When build a value with *, always use 1 for the value of the
;; terminating line, for multiplying by 1 does not change the value of
;; a multiplication.
;;
;; When building a value with cons, always, consider () for the value
;; of terminating line.

null?

zero?

pair?

list?

sub1

add1

car

cdr

eq?
