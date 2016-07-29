#lang racket
(require math)
(require "memo.rkt")

(define (digital-root n)
  (add1 (modulo (sub1 n) 9)))

(define (proper-divisors n)
  (filter (lambda (x) (not (or (= x 1)
                               (= x n))))
          (divisors n)))

(define-memoized (mdrs n)
  (let ([d (proper-divisors n)])
    (cond
      [(null? d) (digital-root n)]
      [else
       (max (digital-root n)
            (argmax values (map (lambda (f)
                                  (+ (mdrs f)
                                     (mdrs (quotient n f))))
                                d)))])))

(for/sum ([n (in-range 2 1000000)])
  (mdrs n))
