#lang racket
(require "memo.rkt")
(require "math.rkt")

; Given 2N elements to allocate between the left and right set, comparing each member
; pairwise, how many combinations have some elements in the left set higher and some in
; the right set higher?
(define (lh-permutations n)
  (let* ([n-As-and-n-Bs
          (/ (factorial (* 2 n)) (square (factorial n)))]
         [matched-As-below-Bs
          (* 2 (catalan n))]
         [mixed-As-and-Bs
          (- n-As-and-n-Bs matched-As-below-Bs)])
    (/ mixed-As-and-Bs 2))) ; Swapping the left and right sets yields duplicate combinations

; Given a set of N elements, combine ways to take K for the left and K for the right set
(define (permutations-k set-size k)
  (* (lh-permutations k)
     (choose set-size (* k 2))))

; Given a set of N elements, test all K-vs-K subset pairs for K=2..N/2
(define (permutations set-size)
  (for/sum ([k (in-range 2 (add1 (quotient set-size 2)))])
    (permutations-k set-size k)))

(permutations 12)
