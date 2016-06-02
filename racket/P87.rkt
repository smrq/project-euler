#lang racket
(require "primes.rkt")
(require data/bit-vector)

(define limit (* 50 1000 1000))
(define primes (primes-up-to (exact-floor (sqrt limit))))

(define limited-squares
  (filter (lambda (n) (< n limit))
          (map (lambda (n) (* n n))
               primes)))
(define limited-cubes
  (filter (lambda (n) (< n limit))
          (map (lambda (n) (* n n n))
               primes)))
(define limited-quads
  (filter (lambda (n) (< n limit))
          (map (lambda (n) (* n n n n))
               primes)))

(define sum-vector (make-bit-vector limit))

(for* ([square limited-squares]
       [cube limited-cubes]
       [quad limited-quads])
  (let ([sum (+ square cube quad)])
    (when (< sum limit)
      (bit-vector-set! sum-vector sum #t))))

(bit-vector-popcount sum-vector)
