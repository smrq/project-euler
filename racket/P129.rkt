#lang racket
(require "repunit.rkt")
(define A smallest-rep-unit-with-divisor)

(define (least-A target)
  ; A(n) < n  ==> start looking above target
  (let loop ([n0 (* (quotient target 10) 10)])
    (cond
      [(> (A (+ n0 1)) target) (+ n0 1)]
      [(> (A (+ n0 3)) target) (+ n0 3)]
      [(> (A (+ n0 7)) target) (+ n0 7)]
      [(> (A (+ n0 9)) target) (+ n0 9)]
      [else (loop (+ n0 10))])))

(least-A (expt 10 6))
