#lang racket
(require "math.rkt")
(require "primes.rkt")

(define primes (primes-up-to 100))

(define (factors->number v)
  (for/product ([i v]
                [p primes])
    (expt p i)))

(define (distinct-solutions v)
  (/ (add1 (for/product ([a v])
             (add1 (* 2 a))))
     2))

(define (log3 n) (/ (log n) (log 3)))

; For the longest time I thought this was supposed to be 4 *BILLION*.
; The answer for that is 506685807472638114647726400, confirmed three or so different ways.
(define target (* 4000000))

(define max-prime-count (exact-ceiling (log3 target)))

(let ([best #f]
      [best-n +inf.0])
  (for ([max-prime-count (range max-prime-count (/ max-prime-count 2) -1)])
    (let ([v (make-vector max-prime-count 1)])
      (for* ([a0 (range 1 10)]
             [a1 (range 1 (add1 a0))]
             [a2 (range 1 (add1 a1))]
             [a3 (range 1 (add1 a2))]
             [a4 (range 1 (add1 a3))])
        (vector-set! v 0 a0)
        (vector-set! v 1 a1)
        (vector-set! v 2 a2)
        (vector-set! v 3 a3)
        (vector-set! v 4 a4)
        (when (and (> (distinct-solutions v) target)
                   (< (factors->number v) best-n))
          (set! best (vector-copy v))
          (set! best-n (factors->number v))))))
  (values best best-n))

;----------------------------------------------------------------------

; This also would have worked, I just had the wrong value for target.

; (define (iterate-vector-at-pos! v position)
;   (for ([i (in-range position)])
;     (vector-set! v i 1))
;   (vector-set! v position (add1 (vector-ref v position))))

; (define (iterate-vector! v best-n)
;   (call/cc (lambda (return)
;     (for ([q (in-range (vector-length v))])
;       (iterate-vector-at-pos! v q)
;       (when (< (factors->number v) best-n)
;         (return #t)))
;     #f)))

; (define (search target)
;   (let* ([best (make-vector (exact-ceiling (log3 target)) 1)]
;          [best-n (factors->number best)])
;     (let loop ([k (sub1 (vector-length best))])
;       (let ([v (make-vector k 1)]
;             [altered? #f])
;         (let inner ()
;           (cond
;             [(iterate-vector! v best-n)
;              (when (> (distinct-solutions v) target)
;                (set! best (vector-copy v))
;                (set! best-n (factors->number v)))
;              (inner)]
;             [(> k 1) (loop (sub1 k))]))))
;     best-n))

; (search target)
