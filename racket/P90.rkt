#lang racket

; 01 04 09 16 25 36 49 64 81

(define (on-die? die n)
  (if (or (= n 6) (= n 9))
      (or (set-member? die 6) (set-member? die 9))
      (set-member? die n)))

(define (pair-on-dice? die1 die2 n1 n2)
  (or (and (on-die? die1 n1) (on-die? die2 n2))
      (and (on-die? die1 n2) (on-die? die2 n1))))

(define combinations
  (for*/list ([a1 (in-range 0         (- 10 5))]
              [b1 (in-range (add1 a1) (- 10 4))]
              [c1 (in-range (add1 b1) (- 10 3))]
              [d1 (in-range (add1 c1) (- 10 2))]
              [e1 (in-range (add1 d1) (- 10 1))]
              [f1 (in-range (add1 e1) (- 10 0))]

              [a2 (in-range 0         (- 10 5))]
              [b2 (in-range (add1 a2) (- 10 4))]
              [c2 (in-range (add1 b2) (- 10 3))]
              [d2 (in-range (add1 c2) (- 10 2))]
              [e2 (in-range (add1 d2) (- 10 1))]
              [f2 (in-range (add1 e2) (- 10 0))])
    (cons (set a1 b1 c1 d1 e1 f1)
          (set a2 b2 c2 d2 e2 f2))))

(/ (for/sum ([combo combinations])
     (let ([die1 (car combo)]
           [die2 (cdr combo)])
       (if (and (pair-on-dice? die1 die2 0 1)
                (pair-on-dice? die1 die2 0 4)
                (pair-on-dice? die1 die2 0 9)
                (pair-on-dice? die1 die2 1 6)
                (pair-on-dice? die1 die2 2 5)
                (pair-on-dice? die1 die2 3 6)
                (pair-on-dice? die1 die2 4 9) ; also 64
                (pair-on-dice? die1 die2 8 1))
           1
           0)))
   2)
