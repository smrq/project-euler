#lang racket

(provide smallest-rep-unit-with-divisor)
(define (smallest-rep-unit-with-divisor n)
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

(provide rep-unit-divides?)
(define (rep-unit-divides? k D)
  (zero? (modulo k (smallest-rep-unit-with-divisor D))))
