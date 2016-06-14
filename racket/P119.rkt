#lang racket
(require "digits.rkt")

(list-ref
  (sort (for*/list ([n (in-range 300)]
                    [k (in-range 50)]
                    #:when (and (= n (digital-sum (expt n k)))
                                (> (expt n k) 9)))
           (expt n k))
        <)
  (sub1 30))

; Brute force - way too slow!
; k=1: 81
; k=2: 512
; k=3: 2401
; k=4: 4913
; k=5: 5832
; k=6: 17576
; k=7: 19683
; k=8: 234256
; k=9: 390625
; k=10: 614656
; k=11: 1679616
; k=12: 17210368
; k=13: 34012224
; k=14: 52521875
; k=15: 60466176
; k=16: 205962976
; k=17: 612220032
; k=18: 8303765625

; (define (is-digit-power-sum? n)
;   (let ([sum (digital-sum n)])
;     (and (zero? (modulo n sum))
;          (let loop ([p sum])
;            (cond
;              [(= p n) #t]
;              [(> p n) #f]
;              [(= sum 1) #f]
;              [else (loop (* p sum))])))))

; (define k-target 30)

; (let loop ([n (add1 512)] [k 2])
;   (if (is-digit-power-sum? n)
;       (if (= (add1 k) k-target)
;           n
;           (begin
;             (displayln (string-append "k=" (~a (add1 k)) ": " (~a n)))
;             (loop (add1 n) (add1 k))))
;       (loop (add1 n) k)))
