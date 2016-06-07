#lang racket

(define (dot v1 v2)
  (+ (* (car v1) (car v2))
     (* (cdr v1) (cdr v2))))

(define (vdiff a b)
  (cons
    (- (car a) (car b))
    (- (cdr a) (cdr b))))

(define (is-right-triangle? v1 v2)
  (or (zero? (dot v1 v2))
      (zero? (dot v1 (vdiff v2 v1)))
      (zero? (dot v2 (vdiff v2 v1)))))

(define limit 50)
(/
  (for*/sum ([x1 (in-range 0 (add1 limit))]
             [y1 (in-range 0 (add1 limit))]
             [x2 (in-range 0 (add1 limit))]
             [y2 (in-range 0 (add1 limit))]
             #:when (and
                      (not (and (zero? x1) (zero? y1)))
                      (not (and (zero? x2) (zero? y2)))
                      (not (and (= x1 x2) (= y1 y2)))))
    (let ([v1 (cons x1 y1)]
          [v2 (cons x2 y2)])
      (if (is-right-triangle? v1 v2)
          1
          0)))
  2)
