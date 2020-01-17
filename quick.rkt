#lang slideshow
(provide rainbow square)

(define (square n)
  (filled-rectangle n n))

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))
