#lang racket
(require "math.rkt")
(require "primes.rkt")

(define (hex-value a b)
  (if (zero? a) 1
      (+ (* 3 a (sub1 a)) b 2)))

(define (potentially-prime-differences a b)
  (cond
    ; top corner
    [(zero? b)
     (list (sub1 (* 6 a))
           (add1 (* 6 a))
           (+ (* 12 a) 5))]

    ; other corner - max 2 primes (half of these are even, half odd)
    ; [(zero? (modulo b a))
    ;  (let ([6a-plus-c (+ (* 6 a)
    ;                      (quotient b a))])
    ;    (list (- 6a-plus-c 6)
    ;          (- 6a-plus-c 1)
    ;          6a-plus-c
    ;          (+ 6a-plus-c 1)))]

    ; top edge
    [(= b (sub1 (* 6 a)))
     (list (- (* 6 a) 1)
           (+ (* 6 a) 5)
           (- (* 12 a) 7))]

    ; other edge - max 2 primes (half of these are even, half odd)
    ; [else
    ;  (let ([6a-plus-c (+ (* 6 a)
    ;                      (quotient b a))])
    ;    (list (- 6a-plus-c 6)
    ;          (- 6a-plus-c 5)
    ;          6a-plus-c
    ;          (+ 6a-plus-c 1)))]
))

(define (PD a b)
  (count is-prime? (potentially-prime-differences a b)))

(define target 2000)
(call/cc (lambda (break)
  (let ([k 9])
    (for* ([a (in-naturals 9)]
           [b (list 0 (sub1 (* 6 a)))]) ; only the top corner and top edge can have more than 2 prime differences
      (when (= (PD a b) 3)
        (set! k (add1 k))
        (displayln (string-append "k=" (~a k) ", a=" (~a a) ", b=" (~a b) ": " (~a (hex-value a b)))))
      (when (= k target) (break (hex-value a b)))))))
