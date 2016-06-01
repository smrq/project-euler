#lang racket
(require data/bit-vector)

(provide bit-vector-none?)

(define (bit-vector-none? v)
  (zero? (bit-vector-popcount v)))
