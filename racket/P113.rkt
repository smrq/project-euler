#lang racket
(require "math.rkt")

(define (increasing-count digits)
  (for/sum ([k (in-range 1 10)])
    (simplex-number k (- digits 1))))

(define (decreasing-count digits)
  (sub1 (for/sum ([k (in-range 1 11)])
    (simplex-number k (- digits 1)))))

(define (not-bouncy-count digits)
  ; overlap between increasing and decreasing is 9 numbers per digit count (1111, 2222, ..., 9999)
  (+ (increasing-count digits)
     (decreasing-count digits)
     -9))

(for/sum ([digits (in-range 1 (add1 100))])
  (not-bouncy-count digits))
