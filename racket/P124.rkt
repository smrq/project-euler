#lang racket
(require "primes.rkt")

(define limit 100000)
(define factors (make-factor-sieve limit))
(define rads
  (for/vector ([f factors])
    (for/product ([n f]) n)))
(define rad-pairs
  (for/list ([i (in-range 1 (add1 limit))])
    (cons i (vector-ref rads i))))

(define (<-pair a b)
  (or (< (cdr a) (cdr b))
      (and (= (cdr a) (cdr b))
           (< (car a) (car b)))))
(define sorted (sort rad-pairs <-pair))

(define (E k)
  (car (list-ref sorted (sub1 k))))

(E 10000)
