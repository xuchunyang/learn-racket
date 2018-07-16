#lang racket

(module+ test
  (require rackunit))

(module+ test
  (require msgpack)
  (test-case "测试 pack/unpack Racket Data"
    (define hodgepodge (vector 1 2 '#(3 #t) "foo"))
    (define packed (call-with-output-bytes (λ (out) (pack hodgepodge out))))
    (define unpacked (call-with-input-bytes packed (λ (in) (unpack in))))
    (check-true (equal? hodgepodge unpacked))))

(module+ test
  (require msgpack-rpc)
  (require racket/async-channel)

  (test-case "测试 msgpack-rpc 客户端"
    ;; $ python msgpack-rpc-server.py
    (define client (start-client "127.0.0.1" 18800 "tcp"))

    ;; 同步
    (match-let ([(list err res)
                 (rpc-call client "sum" 1 2)])
      (check-eqv? 3 res))

    ;; 异步
    (define chan (rpc-call client "sum" 1 2 #:sync? #f))
    (match-let ([(list err res)
                 (async-channel-get chan)])
      (check-eqv? 3 res))))
