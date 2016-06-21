#lang racket
(require racket/trace)

(define (orthogonal-rects a b)
  (for*/sum ([j (in-range 1 (add1 a))]
             [k (in-range 1 (add1 b))])
    (* (add1 (- a j))
       (add1 (- b k)))))

(define (2tri n) (* n (add1 n)))

(define (diagonal-rects-of-size a b j k)
  (+ (* (sub1 a) (sub1 (* 2 b)))
     (sub1 b)
     (- (* (+ j k -2) (+ a b)))
     (2tri (quotient (+ j k -2) 2))
     (2tri (quotient (+ j k -1) 2))
     (if (and (even? j) (even? k)) 1 0)))

(define (diagonal-rects a b)
  (for*/sum ([j (in-range 1 (* 2 (min a b)))]
             [k (in-range 1 (add1 (- (* 2 (min a b)) j)))])
    (diagonal-rects-of-size a b j k)))

(define (rects a b)
  (+ (orthogonal-rects a b)
     (diagonal-rects a b)))

(for*/sum ([a (in-range 1 (add1 47))]
           [b (in-range 1 (add1 43))])
  (rects a b))
