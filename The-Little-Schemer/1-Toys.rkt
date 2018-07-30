#lang racket

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(atom? 'atom)
;; => #t

(atom? 'turkey)
;; => #t

(atom? 1492)
;; => #t

(atom? #\u)
;; => #t

(atom? '*abc$)
;; => #t

(list? '(atom))
;; => #t

(list? '(atom turkey or))
;; => #t

;; (atom turkey) or
;; is not a list

(list? '((atom turkey) or))
;; => #t

;; xyz is an S-expression

;; (x y z) is an S-expression

;; ((x y) z) is an S-expression

(list? '(how are you doing so far))
;; => #t

'(how are you doing so far)
;; has 6 S-expressions in the list

(list? '(((how) are) ((you) (doing so)) far))
;; => #t

'(((how) are) ((you) (doing so)) far)
;; has 3 S-expressions in the list, they are
'((how) are)
'((you) (doing so))
'far

(list? '())
;; => #t

(atom? '())
;; => #f

(list? '(() () () ()))
;; => #t

(car '(a b c))
;; => 'a

(car '((a b c) x y z))
;; => '(a b c)

(require rackunit)
(check-exn exn:fail:contract?
           (λ () (car 'hotdog))
           "You cannot ask for the car of an atom.")

(check-exn exn:fail?
           (lambda () (car '()))
           "You cannot ask for the car of a empty list")


(car '(((hotdogs)) (and) (pickle) relish))
;; => '((hotdogs))

(let ([l '(((hotdogs)) (and) (pickle) relish)])
  (car l))
;; => '((hotdogs))

(car (car '(((hotdogs)) (and))))
;; => '(hotdogs)

(cdr '(a b c))
;; => '(b c)

(cdr '((a b c) x y z))
;; => '(x y z)

(cdr '(hamburger))
;; => '()

(cdr '((x) t r))
;; => '(t r)

(check-exn exn:fail:contract?
           (lambda () (cdr 'hotdogs))
           "You cannot ask for the cdr of an atom")

(check-exn exn:fail:contract?
           (λ () (cdr '()))
           "You cannot ask for the cdr of the null list")

(car (cdr '((b) (x y) ((c)))))
;; => '(x y)

(cdr (cdr '((b) (x y) ((c)))))
;; => '(((c)))

(check-exn exn:fail:contract?
           (λ () (cdr (car '(a (b (c)) d)))))

;; car takes any non-empty list

;; cdr takes any non-empty list

(cons 'peanut '(butter and jelly))
;; => '(peanut butter and jelly)

(cons '(banana and) '(peanut butter and jelly))
;; => '((banana and) peanut butter and jelly)

(cons '((help) this)
      '(is very ((hard) to learn)))
;; => '(((help) this) is very ((hard) to learn))

;; cons takes two arguments
;; - the first is any S-expression
;; - the second is any list

(cons '(a b (c)) '())
;; => '((a b (c)))

(cons 'a '())
;; => '(a)

(cons '((a b c)) 'b)
;; => '(((a b c)) . b)

(cons 'a 'b)
;; => '(a . b)

(cons 'a (car '((b) c d)))
;; => '(a b)

(cons 'a (cdr '((b) c d)))
;; => '(a c d)

(null? '())
;; => #t

(null? (quote ()))
;; => #t

(null? '(a b c))
;; => #f

(null? 'spaghetti)
;; => #f
;; You cannot ask null? of an atom

(atom? 'Harry)
;; => #t

(atom? 'Harry)
;; => #t

(atom? '(Harry had a heap of apples))
;; => #f

;; atom? takes one argument. The argument can be any S-expression

(atom? (car '(Harry had a heap of apples)))
;; => #t

(atom? (cdr '(Harry had a heap of apples)))
;; => #f

(atom? (cdr '(Harry)))
;; => #f

(atom? (car (cdr '(swing low sweet cherry oat))))
;; => #t

(atom? (car (cdr '(swing (low sweet) cherry oat))))
;; => #f

(eq? 'Harry 'Harry)
;; => #t

(eq? 'Harry 'Harry)
;; => #t

(eq? 'margarine 'butter)
;; => #f

;; eq? takes two arguments. both of them must be non-numeric atoms.

(eq? '() '(strawberry))

(eq? 6 7)
;; => #f

(eq? (car '(Mary had a little lamb chop)) 'Mary)
;; => #t

(eq? (cdr '(soured milk)) 'milk)
;; => #f

(let ([l '(beans beans we need jelly beans)])
  (eq? (car l) (car (cdr l))))
;; => #t
