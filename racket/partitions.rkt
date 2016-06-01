#lang racket
(require "memo.rkt")

(provide count-partitions)

(define (nth-pentagonal n)
  (* (/ n 2) (- (* 3 n) 1)))

(define (nth-generalized-pentagonal n)
  (if (even? n)
      (nth-pentagonal (- (/ n 2)))
      (nth-pentagonal (/ (add1 n) 2))))

(define-memoized (count-partitions n)
  (cond
    [(zero? n) 1]
    [(< n 0) 0]
    [else
     (let sum-loop ([i 1] [acc 0])
       (let ([p (nth-generalized-pentagonal i)]) ; https://en.wikipedia.org/wiki/Partition_(number_theory)#Generating_function
         (cond
           [(> p n) acc]
           [(< (remainder (sub1 i) 4) 2)
            (sum-loop (add1 i) (+ acc (count-partitions (- n p))))]
           [else
            (sum-loop (add1 i) (- acc (count-partitions (- n p))))])))]))
