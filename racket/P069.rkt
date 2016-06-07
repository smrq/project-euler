#lang racket

(require "primes.rkt")

(define primes (primes-up-to 1000000))

(define (f)
  (define (g n ls)
    (if (> (* n (car ls)) 1000000)
        n
        (g (* n (car ls)) (cdr ls))))
  (g 1 primes))

(f)
