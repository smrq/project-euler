#lang racket
(require "memo.rkt")

(provide factorial)

(define-memoized (factorial n)
   (if (<= n 1)
       1
       (* n (factorial (sub1 n)))))
