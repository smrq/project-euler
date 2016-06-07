#lang racket

(require "digits.rkt")
(require "primes.rkt")

(define totients (make-totient-sieve 1000000))

(for/sum ([i (in-range 2 (vector-length totients))]) (vector-ref totients i))
