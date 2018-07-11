#lang racket

;; * 1. 类定义
(define fish%
  (class object%
    (init size)                         ; 初始化参数

    (define current-size size)          ; 私有变量

    (super-new)                         ; 初始化父类

    (define/public (get-size)
      current-size)

    (define/public (grow amt)
      (set! current-size (+ amt current-size)))

    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))

;; * 2. 创建类的实例
(define charlie (new fish% [size 10]))

;; * 3. 调用方法
(printf "charlie.get-size is ~a\n" (send charlie get-size))
(send charlie grow 6)
(printf "charlie.get-size is ~a\n" (send charlie get-size))

;; * 4. 子类定义

(define hungry-fish%
  (class fish% (super-new)
    (inherit eat)
    (define/public (eat-more fish1 fish2)
      (eat fish1)
      (eat fish2))))

(let ([a-hungry-fish (new hungry-fish% [size 10])]
      [fish1 (new fish% [size 1])]
      [fish2 (new fish% [size 2])])
  (send a-hungry-fish eat-more fish1 fish2)
  (printf "a-hungry-fish's size is ~a\n" (send a-hungry-fish get-size)))

;; * 5 子类覆盖父类的方法

(define picky-fish%
  (class fish% (super-new)
    (define/override (grow amt)
      ;; 用 super 调用父类的方法
      (super grow (* 3/4 amt)))))

(define daisy (new picky-fish% [size 20]))
(send daisy eat charlie)
(send daisy get-size)

;; * 6 字类覆盖父类初始化参数

(define size-10-fish% (class fish% (super-new [size 10])))

(send (new size-10-fish%) get-size)

(define default-10-fish% (class fish%
                           (init [size 10])
                           (super-new [size size])))

(send (new default-10-fish%) get-size)
(send (new default-10-fish% [size 20]) get-size)

;; * 7. 用 Interface 来确保 class / object 符合要求

(define fish-interface (interface () get-size grow eat))
;; (class* object% (fish-interface))
(class* object% (fish-interface)
  (init size)

  (define current-size size)
  
  (super-new)

  (define/public (get-size) current-size)
  (define/public (grow) (void))
  (define/public (eat) (void)))
