#lang racket

(define max-depth 11)
(define max-target 200)
(define M (make-vector (add1 max-target) +inf.0))

(define (calculate-chain! chain)
  (let ([chain-length (length chain)])
    (when (<= chain-length max-depth)
      (let ([a (car chain)])
        (for ([b chain]
              #:when (<= (+ a b) max-target))
          (when (< chain-length (vector-ref M (+ a b)))
            (vector-set! M (+ a b) chain-length))
          (calculate-chain! (cons (+ a b) chain)))))))

(calculate-chain! '(1))

(for/sum ([i (in-range 2 (add1 max-target))])
  (vector-ref M i))

;-----------------------------------

; This *would* work, but it stalls due to memory just before calculating M(191)=11

; (define (iterate-set expt-set max-target)
;   (let ([a (car expt-set)])
;     (for/set ([b expt-set]
;               #:when (<= (+ a b) max-target))
;       (cons (+ a b) expt-set))))

; (define (iterate-sets expt-sets max-target)
;   (for/fold ([acc (set)])
;     ([sets (set-map expt-sets (lambda (s) (iterate-set s max-target)))])
;     (set-union acc sets)))

; (define base-sets (set '(1)))

; (define (M k-max)
;   (define acc (make-vector (add1 k-max)))
;   (let loop ([m 0]
;              [sets base-sets]
;              [unfound (list->set (range 2 (add1 k-max)))])
;     (displayln (string-append
;                  "Calculating m=" (~a (add1 m))
;                  ", remaining: " (~a unfound)
;                  ", " (~a (set-count sets))
;                  " sets to iterate"))
;     (let* ([sets (iterate-sets sets k-max)]
;            [m (add1 m)]
;            [found (for/set ([n unfound]
;                             #:when (for/or ([s sets]) (= n (car s))))
;                     (vector-set! acc n m)
;                     n)]
;            [unfound (set-subtract unfound found)])
;       (cond
;         [(set-empty? unfound) acc]
;         [else (loop m sets unfound)]))))

; (M 200)
