#lang racket
(require "partitions.rkt")

(let loop ([k 1])
  (let foo ([n (+ (* 5 k) 4)]) ; https://en.wikipedia.org/wiki/Ramanujan%27s_congruences
    (if (zero? (remainder (count-partitions n) 1000000))
        n
        (loop (add1 k)))))
