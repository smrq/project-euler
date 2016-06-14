#lang racket
(require "digits.rkt")
(require "primes.rkt")

(define limit-min (expt 10 9))
(define limit-max (* limit-min 10))

(define max-repeated-digits (make-vector 10 2))
(define sum-repeated-digits (make-vector 10 0))

(define (candidate? v)
  (for/or ([d (in-range 10)])
    (>= (vector-ref v d) (vector-ref max-repeated-digits d))))

(define (count-repeated-digits n)
  (let ([digits-n (digits n)])
    (build-vector 10 (lambda (d) (count (lambda (d2) (= d d2)) digits-n)))))

(for ([n (in-range limit-min limit-max)])
  (when (zero? (modulo n (/ limit-max 10000)))
    (displayln (string-append "Calculating... " (~a n))))
  (let ([repeated (count-repeated-digits n)])
    (when (and (candidate? repeated)
               (is-prime? n))
      (for ([d (in-range 10)])
        (cond
          [(> (vector-ref repeated d) (vector-ref max-repeated-digits d))
           (vector-set! max-repeated-digits d (vector-ref repeated d))
           (vector-set! sum-repeated-digits d n)]
          [(= (vector-ref repeated d) (vector-ref max-repeated-digits d))
           (vector-set! sum-repeated-digits d (+ n (vector-ref sum-repeated-digits d)))])))))

max-repeated-digits
sum-repeated-digits
(for/sum ([s sum-repeated-digits]) s)
