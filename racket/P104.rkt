#lang racket
(require "math.rkt")
(require "memo.rkt")

(define (is-pandigital-string? str)
  (and (string-contains? str "1")
       (string-contains? str "2")
       (string-contains? str "3")
       (string-contains? str "4")
       (string-contains? str "5")
       (string-contains? str "6")
       (string-contains? str "7")
       (string-contains? str "8")
       (string-contains? str "9")))

(define (is-first-pandigital? n)
  (is-pandigital-string? (substring (~a n) 0 9)))

(define (mod-billion n)
  (modulo n 1000000000))

(let loop ([k 2750]
           [fk-2-mod (mod-billion (fibonacci 2748))]
           [fk-1-mod (mod-billion (fibonacci 2749))])
  (let ([fk-mod (mod-billion (+ fk-1-mod fk-2-mod))])
    (when (zero? (modulo k 10000)) (displayln k))
    (if (and (is-pandigital-string? (~a fk-mod))
             (is-first-pandigital? (fibonacci k)))
        k
        (loop (add1 k) fk-1-mod fk-mod))))
