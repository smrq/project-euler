#lang racket
(require "math.rkt")
(require "memo.rkt")

; k' = (s^2)k
; m' = m/s
; n' = n/s
; given s >= 2
; => x' = x, y' = y, z' = z
(define (duplicate? k)
  (let loop ([s 2])
    (let ([ss (square s)])
      (or (divides? k ss)
          (and (not (> ss k))
               (loop (add1 s)))))))

; 1/x + 1/y = 1/z
; x = km(m+n)
; y = kn(m+n)
; z = kmn
(define (distinct-solutions z)
  (for/sum ([mn (divisors z)]
            #:when (not (duplicate? (/ z mn))))
    (exact-ceiling (/ (length (divisors mn)) 2))))

(let loop ([n 4] [best 0])
  (let ([ds (distinct-solutions n)])
    (when (> ds best)
      (displayln (string-append "f(" (~a n) "): " (~a ds))))
    (if (> ds 1000)
        n
        (loop (add1 n) (max ds best)))))

; Records for highest value
; f(1): 1
; f(2): 2
; f(4): 3
; f(6): 5
; f(12): 8
; f(24): 11
; f(30): 14
; f(60): 23
; f(120): 32
; f(180): 38
; f(210): 41
; f(360): 53
; f(420): 68
; f(840): 95
; f(1260): 113
; f(1680): 122
; f(2520): 158
; f(4620): 203
; f(7560): 221
; f(9240): 284
; f(13860): 338
; f(18480): 365
; f(27720): 473
; f(55440): 608
; f(83160): 662
; f(110880): 743
; f(120120): 851
; f(180180): 1013
