#lang racket
(require "math.rkt")
(require "matrix.rkt")

; http://mathworld.wolfram.com/Dice.html
(define (roll-probability count sides value)
  (cond
    [(< value count) 0]
    [(> value (* count sides)) 0]
    [else
     (/
       (for/sum ([k (in-range (add1 (quotient (- value count) sides)))])
         (* (if (odd? k) -1 1)
            (choose count k)
            (choose (sub1 (- value (* sides k))) (sub1 count))))
       (expt sides count))]))

(define roll-distribution-2d6
  (vector
    1/216 ; 3 doubles -> go to jail
    0
    (- 1/36 1/1296)
    2/36
    (- 3/36 1/1296)
    4/36
    (- 5/36 1/1296)
    6/36
    (- 5/36 1/1296)
    4/36
    (- 3/36 1/1296)
    2/36
    (- 1/36 1/1296)))

(define roll-distribution-2d4
  (vector
    1/64 ; 3 doubles -> go to jail
    0
    (- 1/16 1/256)
    2/16
    (- 3/16 1/256)
    4/16
    (- 3/16 1/256)
    2/16
    (- 1/16 1/256)))

(define squares 40)
(define square-go 0)
(define square-jail 10)
(define square-c1 11)
(define square-e3 24)
(define square-h2 39)
(define square-r1 15)
(define cc-list '(2 17 33))
(define ch-list '(7 22 36))
(define g2j-list '(30))
(define transition-matrix (make-matrix squares squares 0))

(define (apply-standard-rolls transition-matrix distribution)
  (for* ([current (in-range (matrix-row-length transition-matrix))]
         [roll-value (in-range (vector-length distribution))])
    (let ([next
           (if (zero? roll-value) ; 3xDoubles
               square-jail
               (mod+ current roll-value (matrix-col-length transition-matrix)))])
      (matrix-set! transition-matrix next current
                   (+ (matrix-ref transition-matrix next current) (vector-ref distribution roll-value))))))

(apply-standard-rolls transition-matrix roll-distribution-2d4)

; Community Chest
(for* ([current (in-range squares)]
       [next cc-list])
  (let ([p (matrix-ref transition-matrix next current)])
    (matrix-set! transition-matrix next current (* p 14/16))
    (matrix-inc! transition-matrix square-go current (* p 1/16))
    (matrix-inc! transition-matrix square-jail current (* p 1/16))))

; Chance
(for* ([current (in-range squares)]
       [next ch-list])
  (let ([p (matrix-ref transition-matrix next current)]
        [square-next-r (cond [(< next 5) 5]
                      [(< next 15) 15]
                      [(< next 25) 25]
                      [(< next 35) 35]
                      [else 5])]
        [square-next-u (cond [(< next 12) 12]
                      [(< next 28) 28]
                      [else 12])]
        [square-back-3 (mod- next 3 squares)])
    (matrix-set! transition-matrix next current (* p 6/16))
    (matrix-inc! transition-matrix square-go current (* p 1/16))
    (matrix-inc! transition-matrix square-jail current (* p 1/16))
    (matrix-inc! transition-matrix square-c1 current (* p 1/16))
    (matrix-inc! transition-matrix square-e3 current (* p 1/16))
    (matrix-inc! transition-matrix square-h2 current (* p 1/16))
    (matrix-inc! transition-matrix square-r1 current (* p 1/16))
    (matrix-inc! transition-matrix square-next-r current (* p 2/16))
    (matrix-inc! transition-matrix square-next-u current (* p 1/16))
    (matrix-inc! transition-matrix square-back-3 current (* p 1/16))))

; Go to Jail
(for* ([current (in-range squares)]
       [next g2j-list])
  (let ([p (matrix-ref transition-matrix next current)])
    (matrix-set! transition-matrix next current 0)
    (matrix-inc! transition-matrix square-jail current p)))

(define (normalize-vector! v)
  (let ([total (for/sum ([n v]) n)])
    (for ([i (in-range (vector-length v))])
      (vector-set! v i (/ (vector-ref v i) total)))))

(let ([probabilities (gaussian-eigenvector! transition-matrix 1)])
  (normalize-vector! probabilities)
  (let* ([pairs (for/list ([i (in-range (vector-length probabilities))])
                  (cons i (exact->inexact (vector-ref probabilities i))))]
         [sorted (sort pairs (lambda (a b) (> (cdr a) (cdr b))))])
    sorted))
