#lang racket
(require "memo.rkt")

(provide square)
(provide cube)
(provide factorial)
(provide choose)
(provide mod+)
(provide mod-)
(provide catalan)

(define (square n) (* n n))
(define (cube n) (* n n n))

(define-memoized (factorial n)
   (if (<= n 1)
       1
       (* n (factorial (sub1 n)))))

(define-memoized (choose n k)
  (cond
    [(> k n) 0]
    [(= n 0) 1]
    [(= k 0) 1]
    [else
     (+ (choose (sub1 n) k)
        (choose (sub1 n) (sub1 k)))]))

(define (mod+ a b m)
  (modulo (+ a b) m))

(define (mod- a b m)
  (modulo (- (+ a m) b) m))

; https://en.wikipedia.org/wiki/Catalan_number
(define (catalan n)
  (/ (choose (* 2 n) n) (add1 n)))
