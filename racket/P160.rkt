#lang racket
(require "memo.rkt")

(define truncate 5)
(define truncate10 (expt 10 truncate))

(define-memoized (f10 N-digits)
  (define (f10-inner N-digits)
    (define N (expt 10 N-digits))

    ; for N >= 10^5, the product of all numbers of the form 10k+{1,3,7,9} mod 10^5 = 1
    ; for N >= 10^5, the product of all numbers of the form  5k+{1,2,3,4} mod 10^5 = 9376
    ; for N >= 10^5, the product of all numbers of the form 10k+{2,4,6,8} mod 10^5 = 9376
    ; Note that 9376^k mod 10^5 = 9376 for any exponent k
    ; Therefore, we can take a factor of 2, 4, 8, or 16 out of the numbers of the form 10k+{1,2,3,4,6,7,8,9} and still yield 9376
    (define acc 9376)

    ; 10k+5 term -> factor out 5 and cancel with a stolen 2 from another term, yields 2k+1
    (let ([extra-2s 0])
      (for ([k (in-range 0 (quotient N 10))])
        (set! extra-2s (+ 1 extra-2s)) ; factor stolen from one of the other terms, e.g. 10k+8 --> 2*(5k+4)
        (let loop ([a (+ (* 2 k) 1)])  ; 10k+5 term, with 5 factored out and canceled by stealing a 2 from another term
          (cond
            [(zero? (modulo a 5))
             (set! extra-2s (sub1 extra-2s))
             (loop (quotient a 5))]
            [else
             (set! acc (modulo (* acc a) truncate10))])))
      (set! acc (modulo (* acc (expt 2 extra-2s)) truncate10)))

    ; 10k+10 term -> factor out 10 yields k+1
    ; product over all k --> (10^(N-digits-1))!
    (set! acc (modulo (* acc (f10 (sub1 N-digits)))
                      truncate10))
    acc)

  ; above algorithm only works for N-digits >= 5
  ; these values were determined from the slow algorithm
  (cond
    [(= N-digits 1) 36288]
    [(= N-digits 2) 16864]
    [(= N-digits 3) 53472]
    [(= N-digits 4) 79008]
    [else (f10-inner N-digits)]))

(f10 12)

; 1 => 36288
; 2 => 16864
; 3 => 53472
; 4 => 79008
; 5 => 62496
; 6 => 12544
; 7 => 94688
; 8 => 54176
; 9 => 38144 (4.5s)
; 10 => 46112 (40.1s)
; 11 =>
; 12 => 16576

;---------------------------------------------

; generalized but slower version of f
; (define (f N)
;   (define acc 1)

;   (for ([k1 (in-range 0 N 10)])
;     (for ([k2 (in-list '(1 3 7 9))])
;       (let ([k (+ k1 k2)])
;         (when (<= k N)
;           (set! acc (modulo (* acc k) truncate10))))))

;   (define extra-2s 0)
;   (for ([k (in-range 0 (add1 (quotient N 10)))])
;     (let ([10k2 (+ (* 10 k) 2)]
;           [10k4 (+ (* 10 k) 4)]
;           [10k6 (+ (* 10 k) 6)]
;           [10k8 (+ (* 10 k) 8)])
;       (when (<= 10k2 N)
;         (set! acc (modulo (* acc (+ (* 5 k) 1)) truncate10)))
;       (when (<= 10k4 N)
;         (set! extra-2s (add1 extra-2s))
;         (set! acc (modulo (* acc (+ (* 5 k) 2)) truncate10)))
;       (when (<= 10k6 N)
;         (set! acc (modulo (* acc 10k6) truncate10)))
;       (when (<= 10k8 N)
;         (set! acc (modulo (* acc 10k8) truncate10)))))

;   (for ([k (in-range 0 (add1 (quotient N 10)))])
;     (when (<= (+ (* 10 k) 5) N)
;       (let loop ([a (+ k 1)])
;         (cond
;           [(zero? (modulo a 5))
;            (set! extra-2s (sub1 extra-2s))
;            (loop (quotient a 5))]
;           [else
;            (set! acc (modulo (* acc a) truncate10))])))
;     (when (<= (+ (* 10 k) 10) N)
;       (let loop ([a (+ (* 2 k) 1)])
;         (cond
;           [(zero? (modulo a 5))
;            (set! extra-2s (sub1 extra-2s))
;            (loop (quotient a 5))]
;           [else
;            (set! acc (modulo (* acc a) truncate10))]))))

;   (set! acc (modulo (* acc (expt 2 extra-2s)) truncate10))
;   acc)

;---------------------------------------------

; (define (f10 N-digits)
;   (define N (expt 10 N-digits))

;   ; for N >= 10^5, the product of all numbers of the form 10k+{1,3,7,9} mod 10^5 = 1
;   ; for N >= 10^5, the product of all numbers of the form  5k+{1,2,3,4} mod 10^5 = 9376
;   ; for N >= 10^5, the product of all numbers of the form 10k+{2,4,6,8} mod 10^5 = 9376
;   ; Note that 9376^k mod 10^5 = 9376 for any exponent k
;   ; Therefore, we can take a factor of 2, 4, 8, or 16 out of the numbers of the form 10k+{1,2,3,4,6,7,8,9} and still yield 9376

;   ; Since somewhere in the calculation we will end up with 9376 as a factor, this could actually be set to 1 and yield the same result!
;   (define acc 9376)

;   (define extra-2s 0)
;   (for ([k (in-range 0 (quotient N 10))])
;     (set! extra-2s (+ 1 extra-2s)) ; factor stolen from one of the other terms, e.g. 10k+8 --> 2*(5k+4)
;     (let loop ([a (+ (* 2 k) 1)]   ; 10k+5 term, with 5 factored out and canceled by stealing a 2 from another term
;                [b (+ k 1)])        ; 10k+10 term, with 10 factored out and canceled
;       (cond
;         [(zero? (modulo a 5))
;          (set! extra-2s (sub1 extra-2s))
;          (loop (quotient a 5) b)]
;         [(zero? (modulo b 5))
;          (set! extra-2s (sub1 extra-2s))
;          (loop a (quotient b 5))]
;         [else
;          (set! acc (modulo (* acc a b) truncate10))])))
;   (set! acc (modulo (* acc (expt 2 extra-2s)) truncate10))
;   acc)

;---------------------------------------------

; (define truncate 5)
; (define truncate10 (expt 10 truncate))

; (define (f10 N-digits)
;   (define N (expt 10 N-digits))
;   (define acc 1)

;   ; this results in a multiplier of 1 for N-digits >= truncate, so we don't need to do this calculation for large inputs
;   (when (< N-digits truncate)
;     (for ([k1 (in-range 0 N 10)])
;       (for ([k2 (in-list '(1 3 7 9))])
;         (let ([k (+ k1 k2)])
;           (set! acc (modulo (* acc k) truncate10))))))

;   (for ([k (in-range 0 (quotient N 10))])
;     (set! acc (modulo (* acc (+ (* 5 k) 1)) truncate10))
;     (set! acc (modulo (* acc (+ (* 10 k) 4)) truncate10))
;     (set! acc (modulo (* acc (+ (* 10 k) 6)) truncate10))
;     (set! acc (modulo (* acc (+ (* 10 k) 8)) truncate10)))

;   (define extra-2s (quotient N 10))
;   (for ([k (in-range 0 (quotient N 10))])
;     (let loop ([a (+ k 1)]
;                [b (+ (* 2 k) 1)])
;       (cond
;         [(zero? (modulo a 5))
;          (set! extra-2s (sub1 extra-2s))
;          (loop (quotient a 5) b)]
;         [(zero? (modulo b 5))
;          (set! extra-2s (sub1 extra-2s))
;          (loop a (quotient b 5))]
;         [else
;          (set! acc (modulo (* acc a b) truncate10))])))
;   (set! acc (modulo (* acc (expt 2 extra-2s)) truncate10))
;   acc)
