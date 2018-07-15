#lang racket

;; $ raco test --drdr persistent-data.rkt
(module+ test
  (require rackunit)
  (test-case "struct? returns #f if struct is not transparent"
    (struct posn (x y))
    (check-false (struct? (posn 1 2))))

  (test-case "struct? returns #t if struct is transparent"
    (struct posn (x y) #:transparent)
    (check-true (struct? (posn 1 2))))

  (test-case "比较两个（不透明的）Struct 实例"
    (struct posn (x y)
      #:methods
      gen:equal+hash
      [(define (equal-proc a b equal?-recur)
         ;; 比较 a 和 b
         (and (equal?-recur (posn-x a) (posn-x b))
              (equal?-recur (posn-y a) (posn-y b))))
       (define (hash-proc a hash-recur)
         ;; 计算 primary hash code of a
         (+ (hash-recur (posn-x a))
            (+ 3 (hash-recur (posn-y a)))))
       (define (hash2-proc a hash2-recur)
         ;; 计算 secondary hash code of a
         (+ (hash2-recur (posn-x a))
            (hash2-recur (posn-y a))))])
    (check-equal? (posn 1 2) (posn 1 2))
    (define h (make-hash))
    (hash-set! h (posn 1 2) 3)
    (check-eqv? (hash-ref h (posn 1 2)) 3)
    (check-exn
     exn:fail:contract?
     (lambda ()
       (hash-ref h (posn 2 1)))))

  (test-case "预先创建 Struct 实例"
    (define p1 #s(pos1 1 3))
    (struct pos1 (x y))
    (check-false (pos1? p1))
    (define p2 #s(pos2 1 3))
    (struct pos2 (x y) #:prefab)
    (check-true (pos2? p2)))

  (test-case "允许修改 Struct"
    (struct dot (x y) #:mutable)
    (define d (dot 1 2))
    (check-eqv? (dot-x d) 1)
    (set-dot-x! d 10)
    (check-eqv? (dot-x d) 10)))
