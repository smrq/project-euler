#lang racket
(require "primes.rkt")

(define (A n)
  (let* ([D9 (cond
               [(= (modulo n 10) 1) (* 9 n)]
               [(= (modulo n 10) 3) (* 3 n)]
               [(= (modulo n 10) 7) (* 7 n)]
               [(= (modulo n 10) 9) n]
               [else (error "n must be coprime to 10")])]
         [m (/ (add1 D9) 10)])
    (let loop ([k 1] [q 1])
      (if (zero? (modulo q n))
          k
          (loop (add1 k) (modulo (+ (* q m) 1) n))))))

(define (has-prime-property? n)
  (zero? (modulo (sub1 n) (A n))))

(define (next n)
  (cond [(= (modulo n 10) 1) (+ n 2)]
        [(= (modulo n 10) 3) (+ n 4)]
        [(= (modulo n 10) 7) (+ n 2)]
        [(= (modulo n 10) 9) (+ n 2)]
        [else (error "n must be coprime to 10")]))

(let loop ([count 0] [n 91] [acc 0])
  (if (and (not (is-prime? n))
           (has-prime-property? n))
      (let ([count (add1 count)]
            [acc (+ acc n)])
        (if (= count 25)
            acc
            (loop count (next n) acc)))
      (loop count (next n) acc)))
