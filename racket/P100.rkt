#lang racket

(define (almost-square n) (* n (sub1 n)))
(define (almost-sqrt n) (/ (add1 (sqrt (add1 (* 4 n)))) 2))
(define limit-min (* 1000 1000 1000 1000))

; Brute force - too slow!
;     Blue |    Total
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
