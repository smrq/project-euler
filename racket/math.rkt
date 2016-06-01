#lang racket
(require "memo.rkt")

(provide factorial)
(provide choose)
(provide mod+)
(provide mod-)

(define-memoized (factorial n)
   (if (<= n 1)
       1
       (* n (factorial (sub1 n)))))

(define (choose a b)
  (/ (factorial a) (* (factorial b) (factorial (- a b)))))

(define (mod+ a b m)
  (modulo (+ a b) m))

(define (mod- a b m)
  (modulo (- (+ a m) b) m))
