#lang racket

;; The First Commandment
;;
;; (preliminary)
;;
;; Always ask null? as the first question in expressing any function

;; The Second Commandment
;;
;; Use cons to build list

;; The Third Commandment
;;
;; When building a list, describe the first typical element, and then
;; cons it onto the natural recursion.

;; The Fourth Commandment
;;
;; (preliminary)
;; 
;; Always change at least one argument while recurring. It must be
;; changed to be closer to the termination condition:
;; when using cdr, test termination with null?

null?

zero?

pair?

list?

sub1

add1

car

cdr

eq?
