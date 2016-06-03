#lang racket
(require racket/trace)
(require data/bit-vector)
(require "digits.rkt")
(require "memo.rkt")

(define (square x) (* x x))
(define (sum ls) (apply + ls))

(define-memoized (iterate n)
  (sum (map square (digits n))))

(define limit (* 10 1000 1000))

(define 89-vector (make-bit-vector limit))
(define visited-vector (make-bit-vector limit))

(bit-vector-set! visited-vector 1 #t)
(bit-vector-set! visited-vector 89 #t)
(bit-vector-set! 89-vector 89 #t)

(define-memoized (visit n)
  (when (not (bit-vector-ref visited-vector n))
    (bit-vector-set! visited-vector n #t)
    (bit-vector-set! 89-vector n (visit (iterate n))))
  (bit-vector-ref 89-vector n))

(for ([i (in-range 1 limit)])
  (displayln i)
  (visit i))

(bit-vector-popcount 89-vector)
