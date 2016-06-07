#lang racket
(require "memo.rkt")

; https://en.wikipedia.org/wiki/Lagrange_polynomial
(define (p fn n)
  (lambda (x)
    (for/sum ([i (in-range 1 (add1 n))])
      (*
        (for/product ([j (in-range 1 (add1 n))]
                         #:when (not (= i j)))
          (/ (- x j)
             (- i j)))
        (fn i)))))

(define (bop fn n)
  ((p fn n) (add1 n)))

(define-memoized (u n)
  (+ 1
     (- n)
     (expt n 2)
     (- (expt n 3))
     (expt n 4)
     (- (expt n 5))
     (expt n 6)
     (- (expt n 7))
     (expt n 8)
     (- (expt n 9))
     (expt n 10)))

(for/sum ([n (in-range 1 (add1 10))])
  (bop u n))
