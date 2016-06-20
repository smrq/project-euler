#lang racket

; Brute force

(require "digits.rkt")

(define (reversible? n)
  (and (not (zero? (modulo n 10))) ; no leading/trailing zeroes
       (let* ([rn (digits->number (reverse (digits n)))]
              [ds (digits (+ n rn))])
         (for/and ([d ds])
           (odd? d)))))

(for/sum ([i (in-range (expt 10 8))] ; no 9-digit numbers
           #:when (reversible? i)) 1)

; 1 digit:  0
; 2 digits: 20
;           2(4) + 2( 3) + 2( 2) + 2( 1)
; 3 digits: 100
;           2(5) + 2(10) + 2(15) + 2(20)
; 4 digits: 600
;           2(2(20)+2(16)+2(12)+2(8)+2(4)) +
;           2(2(15)+2(12)+2( 9)+2(6)+2(3)) +
;           2(2(10)+2( 8)+2( 6)+2(4)+2(2)) +
;           2(2( 5)+2( 4)+2( 3)+2(2)+2(1))
; 5 digits: 0
; 6 digits: 18,000
; 7 digits: 50,000
; 8 digits: 540,000
; 9 digits: 0
