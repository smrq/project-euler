#lang racket
(require "primes.rkt")

(define primes (primes-up-to 1000000))
(define target (expt 10 10))

(let loop ([n 7037]
           [primes (list-tail primes (sub1 7037))])
  (let ([p (car primes)])
    (if (> (* 2 n p) target)
        n
        (loop (+ 2 n) (cddr primes))))) ; for even n, the remainder is always 2 (see P120)
