#lang racket

(require "digits.rkt")
(require "primes.rkt")

(define totients (make-totient-sieve 10000000))

(let ([ls (for/list ([i (in-range 2 (vector-length totients))] #:when (is-permutation? i (vector-ref totients i)))
            (cons i (/ i (vector-ref totients i))))])
  (car (sort ls (lambda (a b) (< (cdr a) (cdr b))))))
