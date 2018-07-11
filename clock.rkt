#lang racket

(define (current-time-string)
  (define (pad v width)
    (~a v
        #:align 'right
        #:width width
        #:pad-string "0"))
  (let ([d (seconds->date (current-seconds))])
    (format "~a-~a-~a ~a:~a:~a"
            (pad (date-year d) 4)
            (pad (date-month d) 2)
            (pad (date-day d) 2)
            (pad (date-hour d) 2)
            (pad (date-minute d) 2)
            (pad (date-second d) 2))))

(module+ main
  (require charterm)
  (with-charterm
    (charterm-clear-screen)
    ;; 找到打印字符串的中心位置
    (define-values (x y)
      (let-values ([(w h) (charterm-screen-size)])
        (values
         (floor (/ (- w (string-length (current-time-string))) 2))
         (floor (/ h 2)))))
    (let loop ()
      (charterm-cursor x y)
      (charterm-display (current-time-string))
      ;; Press any key to quit
      ;; XXX 这个 #:timeout 参数貌似不起作用
      #;
      (unless (charterm-read-key #:timeout 1)
        (loop))
      ;; XXX 这样没法退出，得用 pkill racket
      (sleep 1)
      (loop))))
