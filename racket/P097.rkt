#lang racket

(define (base-power-2 p m)
  (let ([j (modulo (- p m) (* 4 (expt 5 (sub1 m))))])
    (+ m j)))

; Prime = a * 2^p + 1

(define m 10) ; number of digits

(define a 28433)
(define p 7830457) ; power of 2
(define p-eqv (base-power-2 p m))

(modulo
  (add1 (* a (expt 2 p-eqv)))
  (expt 10 m))
