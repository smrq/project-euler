#lang racket

(define singles
  (set-add
    (list->set
      (map (lambda (n) (cons n 1)) (range 1 (add1 20))))
    (cons 25 1)))
(define doubles
  (set-add
    (list->set
      (map (lambda (n) (cons n 2)) (range 1 (add1 20))))
    (cons 25 2)))
(define triples
  (list->set
    (map (lambda (n) (cons n 3)) (range 1 (add1 20)))))
(define all-spaces
  (set-union singles doubles triples))

(define (darts-sum darts)
  (for/sum ([dart darts])
    (* (car dart) (cdr dart))))

(define target-max 100)

(for/sum ([3rd-dart doubles])
  (+ 1
     (for/sum ([2nd-dart all-spaces]
               #:when (< (darts-sum (list 3rd-dart 2nd-dart)) target-max))
       (+ 1
          (for/sum ([1st-dart all-spaces]
                    #:when (< (darts-sum (list 3rd-dart 2nd-dart 1st-dart)) target-max))
            (if (equal? 1st-dart 2nd-dart)
                1
                1/2)))))) ; <-these will get counted twice
