#lang racket

(define (P n)
  (define loss-sets
      (apply append (for/list ([size (in-range (add1 (quotient (sub1 n) 2)))])
                    (combinations (range n) size))))
  (define numerator
    (for/sum ([s loss-sets])
      (for/product ([i s]) (add1 i))))
  (define denominator
    (for/product ([i (in-range n)]) (+ 2 i)))
  (/ numerator denominator))

(define (break-even n)
  (let ([Pn (P n)])
    (exact-floor (/ 1 Pn))))

(break-even 15)
