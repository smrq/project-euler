#lang racket
(require math)

; (define limit 5) ; 35
; (define limit (expt 10 1)) ; 161
; (define limit (expt 10 2)) ; 16749
; (define limit (expt 10 3)) ; 1752541
; (define limit (expt 10 4)) ; 178231226
; (define limit (expt 10 5)) ; 17924657155
; (define limit (expt 10 6)) ; 1795741899159
(define limit (expt 10 8)) ; 17971254122360635

;--------------------------------------------------

(define (calculate-gaussian-sum limit)
  (+ (for/sum ([a (in-range 1 (add1 limit))])
       (* a (quotient limit a)))
     (for/sum ([a (in-range 1 (add1 (exact-floor (sqrt limit))))])
       (for/sum ([b (in-range a 0 -1)]
                 #:when (= 1 (gcd a b)))
         (let ([x (+ (sqr a) (sqr b))])
           (for/sum ([k (in-naturals 1)]
                     #:break (> (* k x) limit))
             (* (quotient limit (* k x))
                k
                (cond
                  [(= a b) (* 2 a)]
                  [else (* 2 (+ a b))]))))))))

(calculate-gaussian-sum limit)

;--------------------------------------------------

; Brute force

; (define (n-multiples-limited a b limit)
;   (let* ([g (gcd a b)]
;          [p (quotient a g)])
;     (and (< (* a p) limit)
;          (let* ([q (quotient b g)]
;                 [x (+ (* a p) (* b q))])
;            (and (<= x limit) x)))))

; (define (calculate-gaussian-sum limit)
;   (+ (for/sum ([a (in-range 1 (add1 limit))])
;        (* a (quotient limit a)))
;      (for/sum ([a (in-range 1 (add1 (quotient limit 2)))])
;        (for/sum ([b (in-range a 0 -1)])
;          (let ([x (n-multiples-limited a b limit)])
;            (if (not x) 0
;                (* (quotient limit x)
;                   (cond
;                     [(= a b) (* 2 a)]
;                     [else (* 2 (+ a b))]))))))))

; (calculate-gaussian-sum limit)
