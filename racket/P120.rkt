#lang racket

; (a-1)^n + (a+1)^n
; =   (nC0)a^n - (nC1)a^1 + ... +- (nCn)a^0
;   + (nC0)a^n + (nC1)a^1 + ... + (nCn)a^0
; n even:
; = 2(nC0)a^n + 2(nC2)a^n-2 + ... + 2(nCn-2)a^2 + 2(nCn)a^0
; = 2(nCn)a^0 (mod a^2)
; = 2 (mod a^2)
; n odd:
; = 2(nC0)a^n + 2(nC2)a^n-2 + ... + 2(nCn-3)a^3 + 2(nCn-1)a^1
; = 2(nCn-1)a^1 (mod a^2)
; = 2na (mod a^2)

; rMax: max n such that 2n < a
(define (rMax a)
  (* 2 a (quotient (sub1 a) 2)))

(for/sum ([a (in-range 3 (add1 1000))])
  (rMax a))
