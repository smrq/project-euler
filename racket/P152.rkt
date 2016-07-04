#lang racket
(require "memo.rkt")

(define-memoized (invsqr n) (/ 1 (sqr n)))

;-------------------------------------

; Potential terms determined from a list of factors of highly composite numbers up through 25200
; The next HCN after that has 11 as a factor
; (define hcns '(2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560 10080 15120 20160 25200))

; All factors of highly-composite numbers up through 25200
; Also equal to the 7-smooth numbers
; (define terms '(2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80))

; How about the 11-smooth numbers?  (Still 154)
; (define terms '(2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32 33 35 36 40 42 44 45 48 49 50 54 55 56 60 63 64 66 70 72 75 77 80))

; How about the 13-smooth numbers?  (This got to 299 before I killed it)
; (define terms '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28 30 32 33 35 36 39 40 42 44 45 48 49 50 52 54 55 56 60 63 64 65 66 70 72 75 77 78 80))

; Subset of the 13-smooth numbers actually used in solutions
(define terms '(2 3 4 5 6 7 8 9 10 12 13 14 15 18 20 21 24 28 30 35 36 39 40 42 45 52 56 60 63 70 72))

; Brute force algorithm using ints with common denominator

(define common-denominator (apply lcm (map sqr terms)))
(define numerators (map (lambda (n) (/ common-denominator (sqr n))) terms))
(define unused-ls
  (let loop ([numerators numerators])
    (if (null? numerators)
        null
        (cons (for/sum ([n numerators]) n)
              (loop (cdr numerators))))))
(define used-terms (mutable-set))
(define (decompose numerators target included unused-ls)
  (cond
    [(null? numerators) 0]
    [(< (car unused-ls) target) 0]
    [(= (car numerators) target)
     (let ([terms (map (lambda (n) (sqrt (/ common-denominator n)))
                       (cons (car numerators) included))])
       (displayln terms)
       (set-union! used-terms (list->set terms))
       (add1 (decompose (cdr numerators) target included (cdr unused-ls))))]
    [(< (car numerators) target)
     (+ (decompose (cdr numerators) (- target (car numerators)) (cons (car numerators) included) (cdr unused-ls))
        (decompose (cdr numerators) target included (cdr unused-ls)))]
    [else
     (decompose (cdr numerators) target included (cdr unused-ls))]))

(decompose numerators (/ common-denominator 2) null unused-ls)

(sort (set->list used-terms) <)

;--------------------------------------

; Brute force algorithm using rationals

; (define unused-ls
;   (let loop ([terms terms])
;     (if (null? terms)
;         null
;         (cons (for/sum ([n terms]) (invsqr n))
;               (loop (cdr terms))))))
; (let loop ([terms terms]
;            [remaining 1/2]
;            [included null]
;            [unused-ls unused-ls])
;   (cond
;     [(null? terms) 0]
;     [(< (car unused-ls) remaining) 0]
;     [else
;      (let ([t-inv (invsqr (car terms))])
;        (cond
;          [(= t-inv remaining)
;           (displayln (cons (car terms) included))
;           (add1 (loop (cdr terms)
;                       remaining
;                       included
;                       (cdr unused-ls)))]
;          [(< t-inv remaining)
;           (+ (loop (cdr terms)
;                    (- remaining t-inv)
;                    (cons (car terms) included)
;                    (cdr unused-ls))
;              (loop (cdr terms)
;                    remaining
;                    included
;                    (cdr unused-ls)))]
;          [else
;           (loop (cdr terms)
;                 remaining
;                 included
;                 (cdr unused-ls))]))]))
