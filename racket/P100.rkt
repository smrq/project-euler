#lang racket
(require "memo.rkt")

(define limit-min (* 1000 1000 1000 1000))

; https://en.wikipedia.org/wiki/Pell_number
(define-memoized (pell n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (* 2 (pell (- n 1)))
             (pell (- n 2)))]))

(define (pell-numerator n)
  (+ (pell n) (pell (sub1 n))))

; B(n)*B(n-1) / T(n)*T(n-1) = 1/2
; ... algebra ...
; (2T(n)-1)^2 - 2(2B(n)-1)^2 = -1
; f(n)^2 - 2g(n)^2 = -1    <-- where f(n) = 2T(n)-1 and g(n) = 2B(n) - 1
; https://en.wikipedia.org/wiki/Pell%27s_equation

(define (blue n)
  (/ (add1 (pell (add1 (* 2 n)))) 2))

(define (total n)
  (/ (add1 (pell-numerator (add1 (* 2 n)))) 2))

(let loop ([i 2])
  (if (> (total i) limit-min)
      (blue i)
      (loop (add1 i))))

;--------------------------------------------------------------------------------------

; Brute force - too slow!
;     Blue |    Total
;        3 |        4
;       15 |       21
;       85 |      120
;      493 |      697
;     2871 |     4060
;    16731 |    23661
;    97513 |   137904
;   568345 |   803761
;  3312555 |  4684660
; 19306983 | 27304197
; ........ | ........

; (define (almost-square n) (* n (sub1 n)))
; (define (almost-sqrt n) (/ (add1 (sqrt (add1 (* 4 n)))) 2))
; (let ([total limit-min]
;       [blue (exact-floor (almost-sqrt (/ (almost-square limit-min) 2)))])
;   (let loop ([total total]
;              [blue blue]
;              [totalSq (almost-square total)]
;              [blueSq (almost-square blue)])
;     (let ([blueSq2 (* 2 blueSq)])
;       (cond
;         [(= totalSq blueSq2) (values blue total)]
;         [(> totalSq blueSq2)
;          (loop total (add1 blue) totalSq (almost-square (add1 blue)))]
;         [else
;          (loop (add1 total) blue (almost-square (add1 total)) blueSq)]))))
