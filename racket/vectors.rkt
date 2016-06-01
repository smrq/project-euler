#lang racket
(require data/bit-vector)

(provide make-vector-by-index)
(provide bit-vector-none?)

(define (make-vector-by-index len fn)
  (apply vector (map fn (build-list len values))))

(define (bit-vector-none? v)
  (zero? (bit-vector-popcount v)))
