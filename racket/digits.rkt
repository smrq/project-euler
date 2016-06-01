#lang racket

(provide digits)
(provide digital-sum)
(provide is-permutation?)

(define (digits n)
  (if (< n 10)
      `(,n)
      (cons (remainder n 10) (digits (quotient n 10)))))

(define (digital-sum n)
  (for/sum ([i (digits n)]) i))

(define (is-permutation? a b)
  (equal? (sort (digits a) <) (sort (digits b) <)))
