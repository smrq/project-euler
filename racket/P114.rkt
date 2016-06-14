#lang racket
(require "memo.rkt")

(define-memoized (count-boxes len min)
  (cond
    [(> min len) 1]
    [else
     (+ (count-boxes (sub1 len) min) ; black box
        (for/sum ([i (in-range min (add1 len))])
          (count-boxes (- len (add1 i)) min)))]))

(count-boxes 50 3)
