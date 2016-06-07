#lang racket
(require "primes.rkt")

(define coprimes (make-coprime-sieve 12000))

(for/sum ([denom (in-range 2 (vector-length coprimes))])
  (let ([ls (vector-ref coprimes denom)])
    (length (filter (lambda (num) (and
                                    (> (/ num denom) (/ 1 3))
                                    (< (/ num denom) (/ 1 2))))
                    ls))))
