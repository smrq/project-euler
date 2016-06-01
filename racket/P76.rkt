#lang racket
(require "memo.rkt")

(define-memoized (count-sums-limited n limit)
  (cond
    [(= n 1) 1]
    [(= limit 1) 1]
    [(>= limit n) (add1 (count-sums-limited n (sub1 n)))]
    [else (for/sum ([i (in-range 1 (add1 (min (sub1 n) limit)))])
            (count-sums-limited (- n i) i))]))

(define (count-sums n)
  (count-sums-limited n (sub1 n)))

(count-sums 100)
