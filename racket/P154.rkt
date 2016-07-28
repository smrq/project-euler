#lang racket
(require "memo.rkt")

(define n 200000)
(define divisor-2 12)
(define divisor-5 12)

(define-memoized (factor-expt n f)
  (cond
    [(zero? n) 0]
    [(zero? (modulo n f))
     (add1 (factor-expt (quotient n f) f))]
    [else 0]))

(define (make-factorial-vector f)
  (let ([factor-vector (build-vector (add1 n) (lambda (x) (factor-expt x f)))]
        [factorial-vector (make-vector (add1 n) 0)])
    (for ([x (in-range 1 (add1 n))])
      (vector-set! factorial-vector x (+ (vector-ref factor-vector x)
                                         (vector-ref factorial-vector (sub1 x)))))
    factorial-vector))

(define facs-2 (make-factorial-vector 2))
(define facs-5 (make-factorial-vector 5))

(let ([target-2 (- (vector-ref facs-2 n) divisor-2)]
      [target-5 (- (vector-ref facs-5 n) divisor-5)])
  (for/sum ([i (in-range 0 (add1 n))])
    (when (zero? (modulo i 1000)) (displayln i))
    (let ([target-2 (- target-2 (vector-ref facs-2 i))]
          [target-5 (- target-5 (vector-ref facs-5 i))]
          [n-i (- n i)])
      (for/sum ([j (in-range 0 (add1 (- n i)))]
                #:when (and (>= target-5 (+ (vector-ref facs-5 j) ; putting the 5 check first has better short-circuiting
                                            (vector-ref facs-5 (- n-i j))))
                            (>= target-2 (+ (vector-ref facs-2 j)
                                            (vector-ref facs-2 (- n-i j))))))
        1))))
