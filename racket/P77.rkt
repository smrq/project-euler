#lang racket
(require "memo.rkt")
(require "primes.rkt")

(define limit 1000)
(define primes (make-bool-sieve limit))

(define-memoized (count-sums-limited n limit)
  (cond
    [(= n 2) (if (>= limit 2) 1 0)]
    [(= n 3) (if (>= limit 3) 1 0)]
    [(and (>= limit n) (sieve-prime? primes n))
     (add1 (count-sums-limited n (sub1 n)))]
    [else
     (for/sum ([i (in-range 2 (add1 (min (sub1 n) limit)))]
               #:when (sieve-prime? primes i))
       (count-sums-limited (- n i) i))]))

(define (count-sums n)
  (count-sums-limited n (sub1 n)))

(let loop ([n 4])
  (let ([sums (count-sums n)])
    (if (> sums 5000)
        `(,n ,sums)
        (loop (add1 n)))))
