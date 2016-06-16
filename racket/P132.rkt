#lang racket
(require "primes.rkt")
(require "repunit.rkt")

(define (coprime-to-10 n)
  (or (= 1 (modulo n 10))
      (= 3 (modulo n 10))
      (= 7 (modulo n 10))
      (= 9 (modulo n 10))))
(define primes (filter coprime-to-10 (primes-up-to 2000000)))

(define (first-prime-factors k factor-count)
  (let loop ([ps primes] [count 0] [acc 0])
    (let* ([p (car ps)] [ps (cdr ps)])
      (if (rep-unit-divides? k p)
          (let ([count (add1 count)]
                [acc (+ p acc)])
            (displayln (string-append (~a count) ": " (~a p)))
            (if (= count factor-count) acc
                (loop ps count acc)))
          (loop ps count acc)))))

(first-prime-factors (expt 10 9) 40)
