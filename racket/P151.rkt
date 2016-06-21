#lang racket
(require "memo.rkt")

(define (sum v) (for/sum ([n v]) n))

(define (cut v n)
  (build-vector (vector-length v)
                (lambda (nn)
                  (cond
                    [(= nn n) (sub1 (vector-ref v nn))]
                    [(> nn n) (add1 (vector-ref v nn))]
                    [else (vector-ref v nn)]))))

(define (pick v)
  (let ([s (sum v)])
    (for/sum ([n (in-range (vector-length v))]
              #:when (not (zero? (vector-ref v n))))
      (* (/ (vector-ref v n) s) (iterate (cut v n))))))

(define (last-iteration? v)
  (and
    (for/and ([n (in-range (sub1 (vector-length v)))])
      (zero? (vector-ref v n)))
    (= 1 (vector-ref v (sub1 (vector-length v))))))

(define-memoized (iterate v)
  (cond
    [(last-iteration? v) 0]
    [(= (sum v) 1) (+ 1 (pick v))]
    [else (pick v)]))

(exact->inexact (pick #(1 0 0 0 0)))
