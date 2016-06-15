#lang racket

(define (L n a b c)
  (+ (* 2 (+ (* a b) (* a c) (* b c)))
     (* 4 n (+ a b c))
     (* 4 n (sub1 n))))

(define limit 50000)

(define v (make-vector limit 0))
(for* ([a (in-range 1 5000)]
       [b (in-range 1 (add1 a))]
       [c (in-range 1 (add1 b))]
       #:when (< (L 0 a b c) limit))
  (let ([Lnabc #f])
    (for ([n (in-range 0 500)]
          #:break (begin
                    (set! Lnabc (L n a b c))
                    (>= Lnabc limit)))
      (vector-set! v Lnabc (add1 (vector-ref v Lnabc))))))

(vector-member 1000 v)
