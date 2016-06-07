#lang racket
(require racket/trace)
(require "memo.rkt")

(define limit 40000)

(define (divides? n m)
  (zero? (modulo n m)))

; http://www.marmet.org/louis/sumprod/index.html
; Crazy on-paper derivation leads to...
(define (g n p)
  (cond
    [(= p 2) (* 2 n)]
    [else (* p (add1 (quotient n (sub1 p))))]))

(define (limited-factorizations? p a factor-count-max total-count sum-target)
  (let loop ([p p] [factor-count-max factor-count-max] [sum a] [count 1])
    (and (not (zero? factor-count-max))
         (or (and (<= p a)
                  (= sum-target (+ sum p (- total-count (add1 count)))))
             (for/or ([i (in-range a 1 -1)])
               (and (divides? p i)
                    (loop (/ p i) (sub1 factor-count-max) (+ sum i) (add1 count))))))))

(define (is-valid-g? n p)
  (or (= p 2)
    (let* ([gnp (g n p)]
           [a (/ gnp p)]
           [factor-count-max (min (+ gnp (- a) (- n) 1)
                                  (+ p a n (- gnp) -1))])
      (limited-factorizations? p a factor-count-max n gnp))))

(define (minimal-product-sum n)
  (let ([best-p 2]
        [best-gnp (g n 2)])
    (for ([p (in-range n 2 -1)])
      (let ([gnp (g n p)])
        (when (and (< gnp best-gnp)
                   (is-valid-g? n p))
          (set! best-p p)
          (set! best-gnp gnp))))
    best-gnp))

; This gets slow, but not impossibly slow
(apply +
       (remove-duplicates
         (for/list ([i (in-range 2 (add1 12000))])
           (displayln i)
           (minimal-product-sum i))))

; Check results up to 10000 against: http://www.marmet.org/louis/sumprod/A104173.html

;--------------------------------------------------------------
; This solution works, but it's very very very slow!

; (define limit (add1 1000))

; ; Calculates the product of every additive partition of a number, after incrementing each member of each partition
; ; (e.g. 4 partitions into 5 (excluded), 4*2, 3*3, 3*2*2, 2*2*2*2)
; (define (partition-product n limit)
;   (cdr (partition-product-limited n n (* 2 limit))))

; (define (within-limit ls limit)
;   (filter (lambda (x) (<= x limit)) ls))

; (define-memoized (partition-product-limited n m limit)
;   (cond
;     [(= m 1) (within-limit (list (expt 2 n)) limit)]
;     [(>= m n) (cons (add1 n) (partition-product-limited n (sub1 n) limit))]
;     [else
;      (apply append
;             (for/list ([i (in-range m 0 -1)])
;               (within-limit (map (lambda (x) (* x (add1 i))) (partition-product-limited (- n i) i limit)) limit)))]))

; (define minimal-product-sum-vector (make-vector limit))

; (for ([k (in-range 2 limit)])
;   (displayln k)
;   (for ([n (partition-product k limit)])
;     (let ([i (- n k)])
;       (when (and (< i limit) (zero? (vector-ref minimal-product-sum-vector i)))
;         (vector-set! minimal-product-sum-vector i n)))))

; (displayln "----")

; (for/sum ([n
;            (remove-duplicates
;              (for/list ([i (in-range 2 limit)])
;                (vector-ref minimal-product-sum-vector i)))])
;   n)
