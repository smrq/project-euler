#lang racket

; (define max-row 6)
; (define cells (vector 15 -14 -7 20 -13 -5 -3 8 23 -26 1 -4 -5 -18 5 -16 31 2 9 28 3))

(define max-row 1000)
(define cells (make-vector (* max-row (add1 max-row) 1/2) #f))
(let loop ([t 0]
           [k 0])
  (when (< k (vector-length cells))
    (let ([t (modulo (+ (* 615949 t)
                        797807)
                     (expt 2 20))])
      (vector-set! cells k (- t (expt 2 19)))
      (loop t (add1 k)))))

(define (rc->i r c)
  (+ c (* r (add1 r) 1/2)))

(define (lookup r c)
  (let ([i (rc->i r c)])
    (if (< i (vector-length cells))
        (vector-ref cells i)
        (cons 0 null))))

(define (merge self next-a next-b next-shared)
  (let ([best-a (car next-a)]
        [best-b (car next-b)]
        [next-a (cdr next-a)]
        [next-b (cdr next-b)]
        [next-shared (cdr next-shared)])
    (let ([merged
           (for/list ([a (cons 0 next-a)]
                      [b (cons 0 next-b)]
                      [s (cons 0 (cons 0 next-shared))])
                   (+ self a b (- s)))])
      (values (min best-a best-b (apply min merged))
              merged))))

(for* ([r (in-range (sub1 max-row) -1 -1)]
       [c (in-range 0 (add1 r))])
  (let* ([i (rc->i r c)]
         [self (vector-ref cells i)]
         [next-a (lookup (add1 r) c)]
         [next-b (lookup (add1 r) (add1 c))]
         [next-shared (lookup (+ r 2) (add1 c))])
    (let-values ([(best merged) (merge self next-a next-b next-shared)])
      (vector-set! cells i (cons best merged)))))

(car (vector-ref cells 0))
