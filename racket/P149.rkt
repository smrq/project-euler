#lang racket
(require "matrix.rkt")

(define grid (make-matrix 2000 2000 #f))
(for ([k (in-range 1 56)])
  (matrix-set! grid 0 (sub1 k)
    (- (modulo (+ 100003 (* -200003 k) (* 300007 k k k))
                                     1000000)
       500000)))
(for ([k (in-range 56 (add1 (* 2000 2000)))])
  (let* ([i (quotient (sub1 k) 2000)]
         [j (modulo (sub1 k) 2000)]
         [i-24 (quotient (- k 25) 2000)]
         [j-24 (modulo (- k 25) 2000)]
         [sk-24 (matrix-ref grid i-24 j-24)]
         [i-55 (quotient (- k 56) 2000)]
         [j-55 (modulo (- k 56) 2000)]
         [sk-55 (matrix-ref grid i-55 j-55)])
    (matrix-set! grid i j
                      (- (modulo (+ sk-24 sk-55 1000000)
                                 1000000)
                         500000))))

(define (greatest-horizontal grid)
  (for/fold ([best 0])
            ([i (in-range (matrix-row-length grid))])
    (let-values ([(current _)
                  (for/fold ([best 0] [current 0])
                            ([j (in-range (matrix-col-length grid))])
                    (let* ([value (matrix-ref grid i j)]
                           [added (+ current value)])
                      (cond
                        [(> added best) (values added added)]
                        [(> added 0) (values best added)]
                        [else (values best 0)])))])
      (max best current))))


(define (greatest-vertical grid)
  (for/fold ([best 0])
            ([j (in-range (matrix-row-length grid))])
    (let-values ([(current _)
                  (for/fold ([best 0] [current 0])
                            ([i (in-range (matrix-col-length grid))])
                    (let* ([value (matrix-ref grid i j)]
                           [added (+ current value)])
                      (cond
                        [(> added best) (values added added)]
                        [(> added 0) (values best added)]
                        [else (values best 0)])))])
      (max best current))))

(define (greatest-diagonal grid)
  (for/fold ([best 0])
            ([i0 (in-range (add1 (- (matrix-col-length grid)))
                           (matrix-row-length grid))])
    (let-values ([(current _)
                  (for/fold ([best 0] [current 0])
                            ([i (in-range i0 (+ i0 (matrix-col-length grid)))]
                             [j (in-range (matrix-col-length grid))]
                             #:when (and (>= i 0)
                                         (< i (matrix-row-length grid))))
                    (let* ([value (matrix-ref grid i j)]
                           [added (+ current value)])
                      (cond
                        [(> added best) (values added added)]
                        [(> added 0) (values best added)]
                        [else (values best 0)])))])
      (max best current))))

(define (greatest-anti-diagonal grid)
  (for/fold ([best 0])
            ([i0 (in-range (+ (matrix-col-length grid)
                              (matrix-row-length grid)))])
    (let-values ([(current _)
                  (for/fold ([best 0] [current 0])
                            ([i (in-range i0 (- i0 (matrix-col-length grid)) -1)]
                             [j (in-range (matrix-col-length grid))]
                             #:when (and (>= i 0)
                                         (< i (matrix-row-length grid))))
                    (let* ([value (matrix-ref grid i j)]
                           [added (+ current value)])
                      (cond
                        [(> added best) (values added added)]
                        [(> added 0) (values best added)]
                        [else (values best 0)])))])
      (max best current))))

(define (greatest-subsequence grid)
  (max (greatest-horizontal grid)
       (greatest-vertical grid)
       (greatest-diagonal grid)
       (greatest-anti-diagonal grid)))

(greatest-subsequence grid)
