#lang racket
(require "pythagorean-triples.rkt")
(require "math.rkt")

; (triple-find-pattern
;   (lambda (a b c k) (divides? c (abs (- b a))))
;   15 1)

(define limit (expt 10 8))
(let loop ([triple (first base-triples)] [acc 0])
  (let* ([a (first triple)]
         [b (second triple)]
         [c (third triple)]
         [perimeter (+ a b c)]
         [k-max (quotient limit perimeter)])
    (if (> k-max 0)
        (loop (next-triple-b triple) (+ acc k-max))
        acc)))
