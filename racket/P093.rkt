#lang racket
(require "memo.rkt")

(define-memoized (combine a b)
  (set-union
    (set
      (+ a b)
      (- a b)
      (- b a)
      (* a b))
    (if (zero? a) (set) (set (/ b a)))
    (if (zero? b) (set) (set (/ a b)))))

(define (combine-set a ls)
  (apply set-union (for/list ([b ls]) (combine a b))))

(define-memoized (run-rational numbers)
  (cond
    [(= (set-count numbers) 1) numbers]
    [else
     (apply set-union
            (for/list ([n numbers])
              (let ([ls (run-rational (set-subtract numbers (set n)))])
                (combine-set n ls))))]))

(define (positive-integer? x)
  (and (integer? x)
       (> x 0)))

(define (run numbers)
  (sort (filter positive-integer? (set->list (run-rational numbers))) <))

(define (score numbers)
  (let loop ([numbers numbers] [i 1])
    (if (= (car numbers) i)
        (loop (cdr numbers) (add1 i))
        (sub1 i))))

(let ([best-set null]
      [best-score 0])
  (for* ([d (in-range 4 10)]
         [c (in-range 3 d)]
         [b (in-range 2 c)]
         [a (in-range 1 d)])
    (let foo ([current-score (score (run (set a b c d)))])
      (when (> current-score best-score)
        (set! best-score current-score)
        (set! best-set (list a b c d)))))
  best-set)
