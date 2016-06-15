#lang racket
(require "digits.rkt")

(define (square n) (* n n))

(define (consecutive-square-sum start end)
  (for/sum ([n (map square (range start (add1 end)))])
    n))

(define limit (expt 10 8))
; (define limit (expt 10 3))

(define (consecutive-square-sums limit)
  (let loop-i ([i 1] [acc null])
    (let loop-j ([j (add1 i)] [acc acc])
      (let ([css (consecutive-square-sum i j)])
        (cond
          [(> css limit)
           (if (= j (add1 i))
               acc
               (loop-i (add1 i) acc))]
          [else
           (loop-j (add1 j) (cons css acc))])))))

(define (is-palindrome-number? n)
  (let ([d (digits n)])
    (equal? d (reverse d))))

(define palindromes (filter is-palindrome-number? (consecutive-square-sums limit)))

(for/sum ([p (list->set palindromes)]) p)
