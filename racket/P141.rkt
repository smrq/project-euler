#lang racket

; n = m^2 = dq + r
; Assume d > q

; Pretend r > q
; d/r = r/q
; r^2 = dq
; m^2 = r^2 + r = r(r+1)  ==> contradiction
; Therefore r < q
; d/q = q/r
; dr = q^2
; r = q^2 / d
; m^2 = dq + q^2/d

; Pretend q > m
; m^2 = dq + q^2/d > q^2 + q^2/q > m^2 + m  ==> contradiction
; Therefore q <= m

; Pretend q = m
; m^2 = dq + q^2/d = dm + m^2/d
; m = d + m/d
; dm = d^2 + m
; (d-1)m = d^2
; m = d^2/(d-1)  ==> contradiction
; Therefore q < m

; m^2 = q(d + q/d)
; q < m ==> d + q/d > m
; q/d < 1 ==> d > m
; Therefore d > m > q > r

;------------------------------------------

; Brute force

; (define limit (expt 10 12))
; (define limit-sqrt (sqrt limit))

; (for/sum ([m (in-range 1 (add1 limit-sqrt))])
;   (let ([n (sqr m)])
;     (if (for/or ([q (in-range 2 m)])
;           (let-values ([(d r) (quotient/remainder n q)])
;             (= (* d r) (sqr q))))
;         n
;         0)))

; 9
; 10404
; 16900
; 97344
; 576081
; 6230016
; 7322436
; 12006225
; 36869184
; 37344321
; 70963776
; 196112016
; 256160025
; 1361388609
; 1380568336
; 8534988225
; 9729849600
; 12551169024
; 13855173264
; 16394241600

;------------------------------------------

; Brute force II

; d = a/b * q = a^2/b^2 * r
; q = a/b * r
; a > b, a,b coprime
; b^2 | r
; r = kb^2
; q = kab
; d = ka^2

; m^2 = dq+r
;     = (ka^2)(kab) + (kb^2)
;     = kb(ka^3 + b)

(define limit (expt 10 12))
(define limit-sqrt (sqrt limit))
(define limit-cubert (exact-round (expt limit 1/3)))

(define (coprime? a b)
  (= 1 (gcd a b)))

(define (perfect-square? n)
  (exact? (sqrt n)))

(for*/sum ([a (in-range 2 limit-cubert)]
           [b (in-range 1 a)]
           #:when (coprime? a b))
  (let ([n #f])
    (for/sum ([k (in-naturals 1)]
              #:break (begin
                        (set! n (* k b (+ (* k a a a) b)))
                        (> n limit))
              #:when (perfect-square? n))
      (displayln (string-append "--- " (~a n)))
      n)))
