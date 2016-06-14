#lang racket
(require "digits.rkt")

(define (decreasing? n)
  (let loop ([d (digits n)])
    (or
      (null? d)
      (null? (cdr d))
      (and (not (< (car d) (cadr d)))
           (loop (cdr d))))))

(define (increasing? n)
  (let loop ([d (digits n)])
    (or
      (null? d)
      (null? (cdr d))
      (and (not (> (car d) (cadr d)))
           (loop (cdr d))))))

(define (bouncy? n)
  (and (not (decreasing? n))
       (not (increasing? n))))

(let loop ([n 1]
           [bouncy 0])
  (let ([bouncy (if (bouncy? n) (add1 bouncy) bouncy)])
    (if (= (* 100 bouncy) (* 99 n))
        n
        (loop (add1 n) bouncy))))
