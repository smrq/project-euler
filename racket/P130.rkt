#lang racket
(require "primes.rkt")
(require "repunit.rkt")

(define (has-prime-property? n)
  (rep-unit-divides? (sub1 n) n))

(define (next n)
  (cond [(= (modulo n 10) 1) (+ n 2)]
        [(= (modulo n 10) 3) (+ n 4)]
        [(= (modulo n 10) 7) (+ n 2)]
        [(= (modulo n 10) 9) (+ n 2)]
        [else (error "n must be coprime to 10")]))

(let loop ([count 0] [n 91] [acc 0])
  (if (and (not (is-prime? n))
           (has-prime-property? n))
      (let ([count (add1 count)]
            [acc (+ acc n)])
        (if (= count 25)
            acc
            (loop count (next n) acc)))
      (loop count (next n) acc)))
