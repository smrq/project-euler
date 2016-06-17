#lang racket

; n = Af(x) = x / (1 - x - x^2)
; x = (-n - 1 + sqrt(5n^2 + 2n + 1)) / 2n
; x is rational iff (5n^2 + 2n + 1) is a perfect square

;-----------------------------------------------------------

; (5n^2 + 2n + 1) = (n + 1)^2 + (2n)^2
; => Triples where b = 2a - 2

; Find pattern in triple tree for this invariant

; (require "pythagorean-triples.rkt")
; (define (f? a b c k)
;   (= (* k b) (- (* k 2 a) 2)))
; (triple-find-pattern f? 15 2)

; k=1: x
; k=1: CBAx
; k=1: CBCBCBAx
; k=1: CBCBCBCBCBAx
; k=1: CBCBCBCBCBCBCBAx
; k=1: CBCBx
; k=1: CBCBCBCBx
; k=1: CBCBCBCBCBCBx
; k=2: Cx
; k=2: CBCBCx
; k=2: CBCBCBCBCx
; k=2: CBCBCBCBCBCBCx
; ===>
; Start with {base for k=1, C*B*A*base for k=1, C*base for k=2}
; Iterate: C*B*C*B*base

;-----------------------------------------------------------

; Wtf?????? A081018
(require "math.rkt")
(define (N q)
  (* (fibonacci (* 2 q))
     (fibonacci (add1 (* 2 q)))))
(N 15)

;-----------------------------------------------------------

; Wtf?
; (require "math.rkt")
; (define (N q)
;   (let ([k (fibonacci (add1 (* 4 q)))]) ; <-- why??????
;     (/ (sub1 (sqrt (- (* 5 (sqr k)) 4))) 5)))
; (N 15)

; ----------------------------------------------------------

; Too slow!!
; (define (perfect-square? n)
;   (exact? (sqrt n)))
; (let loop ([count 0] [n 1])
;   (if (perfect-square? (+ (* 5 n n) (* 2 n) 1))
;       (let ([count (add1 count)])
;         (displayln (string-append (~a count) ": " (~a n)))
;         (if (= count 15)
;             n
;             (loop count (add1 n))))
;       (loop count (add1 n))))
