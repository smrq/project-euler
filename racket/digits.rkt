#lang racket

(provide digits)
(provide is-permutation?)

(define (digits n)
  (if (< n 10)
      `(,n)
      (cons (remainder n 10) (digits (quotient n 10)))))

(define (is-permutation? a b)
  (equal? (sort (digits a) <) (sort (digits b) <)))
