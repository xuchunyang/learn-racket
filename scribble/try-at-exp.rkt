#lang at-exp racket

(require rackunit)

(define v '@op{str})
(define v2 '(op "str"))

(check-equal? v v2)



'@foo{blah blah blah}

'@foo{eli@"@"barzilay.org}

'@C{while (*(p++)) {

     *p = '\n';

   }}
