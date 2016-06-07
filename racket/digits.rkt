#lang racket

(provide digits)
(provide digits->number)
(provide number-length)
(provide digital-sum)
(provide is-permutation?)

(define (digits n)
  (let loop ([n n] [acc null])
    (if (< n 10)
        (cons n acc)
        (loop (quotient n 10) (cons (remainder n 10) acc)))))

(define (digits->number ds)
  (let loop ([ds ds] [acc 0])
    (if (null? ds)
        acc
        (loop (cdr ds) (+ (* 10 acc) (car ds))))))

(define (number-length n)
  (let loop ([n n] [acc 0])
    (if (> n 0)
        (loop (quotient n 10) (add1 acc))
        acc)))

(define (digital-sum n)
  (for/sum ([i (digits n)]) i))

(define (is-permutation? a b)
  (equal? (sort (digits a) <) (sort (digits b) <)))
