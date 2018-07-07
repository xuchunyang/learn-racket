#lang racket/base

(let rw ([b (read-byte)])
  (unless (eof-object? b)
    (write-byte b)
    (rw (read-byte))))
