#lang racket/gui

(require (file "~/gist/ydcv.rkt"))

(define frame (new frame%
                   [label "有道词典"]
                   [width 400]
                   [height 300]))

(define text-field (new text-field%
                        [parent frame]
                        [label #f]
                        [callback
                         (lambda (text-field event)
                           (when (eq? (send event get-event-type) 'text-field-enter)
                             (let ([input (send text-field get-value)])
                               (send message set-label "Searching...")
                               (send message set-label (string-join (explains input) "\n")))))]))

(define message (new message%
                     [parent frame]
                     [label "目前没有结果"]
                     [auto-resize #t]))

(send frame center)
(send frame show #t)
