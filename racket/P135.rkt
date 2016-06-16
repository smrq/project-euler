#lang racket

; x^2 - y^2 - z^2 = n
; let: x = a + 2k
;      y = a + k
;      z = a
; 3k^2 + 2ak - a^2 = n
; (3k-a)(k+a) = n
;  -> a < 3k

; N is at maximum at a=k

(define (N k a)
  (+ (* 3 k k) (* 2 a k) (* -1 a a)))

(define limit 1000000)
(define v (make-vector limit))

(for ([k (in-naturals 1)]
      #:break (>= (N k (sub1 (* 3 k))) limit))

  (for ([a (in-range (sub1 (* 3 k)) (sub1 k) -1)]
        #:break (>= (N k a) limit))
    (let ([n (N k a)])
      (vector-set! v n (add1 (vector-ref v n)))))

  (for ([a (in-range 1 k)]
        #:break (>= (N k a) limit))
    (let ([n (N k a)])
      (vector-set! v n (add1 (vector-ref v n))))))

(for/sum ([count v])
  (if (= count 10) 1 0))
