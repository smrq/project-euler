#lang racket
(require racket/trace)

(define (dot v1 v2)
  (+ (* (car v1) (car v2))
     (* (cdr v1) (cdr v2))))

(define (vec+ v1 v2)
  (cons (+ (car v1) (car v2))
        (+ (cdr v1) (cdr v2))))

(define (vec- v1 v2)
  (cons (- (car v1) (car v2))
        (- (cdr v1) (cdr v2))))

(define (vec* n v)
  (cons (* n (car v))
        (* n (cdr v))))

(define (vec/ v n)
  (cons (/ (car v) n)
        (/ (cdr v) n)))

(define (normalize v)
  (vec/ v (sqrt (dot v v))))

(define (reflect p0 p1)
  (let* ([v1 (vec- p1 p0)]
         [n (cons (* -4 (car p1))
                  (- (cdr p1)))]
         [n^ (normalize n)]
         [v2 (vec- v1
                   (vec* (* 2 (dot v1 n^)) n^))]
         [a (+ (* 4 (sqr (car v2))) (sqr (cdr v2)))]
         [b (+ (* 8 (car p1) (car v2))
               (* 2 (cdr p1) (cdr v2)))]
         [c (+ (* 4 (sqr (car p1)))
               (sqr (cdr p1))
               -100)]
         [k (/ (+ (- b) (sqrt (- (sqr b) (* 4 a c))))
               (* 2 a))]
         [p2 (vec+ p1 (vec* k v2))])
    p2))

(let loop ([p0 '(0 . 10.1)]
           [p1 '(1.4 . -9.6)]
           [times 1])
  (let ([p2 (reflect p0 p1)])
    (displayln p2)
    (if (and (>= (car p2) -0.01)
             (<= (car p2) 0.01)
             (> (cdr p2) 0))
        times
        (loop p1 p2 (add1 times)))))
