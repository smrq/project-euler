#lang racket
(require "memo.rkt")

(define-memoized (count-boxes len min)
  (cond
    [(> min len) 1]
    [else
     (+ (count-boxes (sub1 len) min) ; black box
        (for/sum ([i (in-range min (add1 len))])
          (count-boxes (- len (add1 i)) min)))]))

(let loop ([n 51])
  (if (> (count-boxes n 50) 1000000)
      n
      (loop (add1 n))))
