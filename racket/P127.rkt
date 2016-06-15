#lang racket
(require "memo.rkt")
(require "primes.rkt")

(define (product ls)
  (for/product ([n ls]) n))

(define limit 120000)
(define factors (vector-map list->set (make-factor-sieve limit)))
(define rads (vector-map product factors))

(define-memoized (coprime? a b)
  (set-empty? (set-intersect (vector-ref factors a)
                             (vector-ref factors b))))

(define-memoized (coprime-abc? a b c)
  (and (coprime? a b)
       (coprime? a c)
       (coprime? b c)))

(define (rad-coprimes a b c)
  (* (vector-ref rads a)
     (vector-ref rads b)
     (vector-ref rads c)))

(for*/sum ([a (in-range 1 (/ limit 2))]
           [b (in-range (add1 a) (- limit a))]
           #:when (and (< (+ a b) limit)
                       (< (rad-coprimes a b (+ a b)) (+ a b))
                       (coprime-abc? a b (+ a b))))
  (+ a b))
