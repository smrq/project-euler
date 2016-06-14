#lang racket
(require "memo.rkt")

(define-memoized (count-boxes len)
  (cond
    [(< len 0) 0]
    [(= len 0) 1]
    [else (+ (count-boxes (sub1 len))
             (count-boxes (- len 2))
             (count-boxes (- len 3))
             (count-boxes (- len 4)))]))

(count-boxes 50)
