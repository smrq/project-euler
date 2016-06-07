#lang racket
(require "digits.rkt")

; https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Decimal_.28base_10.29
(define (sqrt-decimals n limit)
  (let loop ([r n] [p 0] [limit (sub1 limit)])
    (if (or (zero? limit) (zero? r))
        p
      (let ([c (* 100 r)])
        (let inner-loop ([x 0])
          (if (<= (* (add1 x) (+ (* 20 p) (add1 x))) c)
              (inner-loop (add1 x))
              (let* ([y (* x (+ (* 20 p) x))]
                     [r2 (- c y)]
                     [p2 (+ (* 10 p) x)])
                (loop r2 p2 (sub1 limit)))))))))

(define limit 100)
(for/sum ([n (in-range 1 (add1 limit))])
  (let ([sqrt-digits (digits (sqrt-decimals n limit))])
    (if (< (length sqrt-digits) 100)
        0
        (for/sum ([i sqrt-digits]) i))))
