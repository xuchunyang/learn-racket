#lang racket/base

(define (a-long-time-job)
  (sleep 5))

(define t (thread a-long-time-job))

(let loop ([start (current-seconds)])
  (when (thread-running? t)
    (sleep 1)
    (printf "Running ~a seconds\n" (- (current-seconds) start))
    (loop start)))
