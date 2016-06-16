#lang racket
(require "primes.rkt")
(require "math.rkt")

(define (next-power-of-10 n)
  (let loop ([p 10])
    (if (> p n) p (loop (* 10 p)))))

(define (ends-with? big end)
  (= end (modulo big (next-power-of-10 end))))

(define primes (cddr (primes-up-to 1000003)))

(define (N p1 p2)
  (let ([mod (next-power-of-10 p1)]
        [target-last-digit (modulo p1 10)])
    (let loop ([count 1] [q (modulo p2 mod)])
      (cond
        [(= q p1) (* count p2)]
        [(= (modulo q 10) target-last-digit)
         (loop (+ 10 count) (mod+ q (* 10 p2) mod))]
        [else
         (loop (add1 count) (modulo (+ q p2) mod))]))))

(for/sum ([p1 primes]
          [p2 (cdr primes)])
  (N p1 p2))
