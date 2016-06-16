#lang racket
(require "primes.rkt")
(require "repunit.rkt")

(define (coprime-to-10 n)
  (or (= 1 (modulo n 10))
      (= 3 (modulo n 10))
      (= 7 (modulo n 10))
      (= 9 (modulo n 10))))

(define primes (primes-up-to 100000))
(define factors (make-factor-sieve 100000))

(for/sum ([p primes]
          #:when (or (not (coprime-to-10 p))
                     (let* ([k (smallest-rep-unit-with-divisor p)]
                            [f (vector-ref factors k)])
                       (not (or (equal? f '(2))
                                (equal? f '(5))
                                (equal? f '(5 2)))))))
  p)
