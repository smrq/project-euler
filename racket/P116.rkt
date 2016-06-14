#lang racket
(require "memo.rkt")

(define-memoized (count-boxes len size)
  (cond
    [(< len size) 1]
    [else (+ (count-boxes (sub1 len) size)
             (count-boxes (- len size) size))]))

(+ (sub1 (count-boxes 50 2))
   (sub1 (count-boxes 50 3))
   (sub1 (count-boxes 50 4)))
