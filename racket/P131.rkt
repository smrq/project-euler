#lang racket
(require "primes.rkt")

(define (cube n) (* n n n))

(define limit (expt 10 6))
(define primes (primes-up-to limit))

(define perfect-cubes (mutable-set))
(define perfect-cubes-highest-n 0)
(define perfect-cubes-highest 0)

; Ensures the precomputed set of cubes contains all cubes up to at least x
(define (expand-cube-set x)
  (let loop ()
    (when (> x perfect-cubes-highest)
      (set! perfect-cubes-highest-n (add1 perfect-cubes-highest-n))
      (set! perfect-cubes-highest (cube perfect-cubes-highest-n))
      (set-add! perfect-cubes perfect-cubes-highest)
      (loop))))

(define (perfect-cube? n)
  (expand-cube-set n)
  (set-member? perfect-cubes n))

(define (has-partner? p)
  (let loop ([cube-root-n 1] [n 1])
    (or (perfect-cube? (+ n p))
        (let ([next-n (cube (add1 cube-root-n))])
          (and (> p (- next-n n))
               (loop (add1 cube-root-n) next-n))))))

(define partnered-primes
  (for/list ([p primes]
             #:when (has-partner? p))
    p))

partnered-primes
(length partnered-primes)
