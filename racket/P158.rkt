#lang racket

;  2: 325
;  3: 10400
;  4: 164450
;  5: 1710280 (1.1s)
;  6: 13123110 (6.5s)
;  7: 78936000 (34.6s)
;  8: 385881925 (180.7s)
;  9: 1568524100 (paper)
; ...

;-------------------------------------------------

(define (calculate-pos n position)
  (let* ([used (set)])
    (let loop ([ls null] [p 0] [used used])
      (cond
        [(= p n) 1]
        [(= p position)
         (for/sum ([x (in-range (add1 (car ls)) 26)]
                   #:when (not (set-member? used x)))
           (loop (cons x ls)
                 (add1 p)
                 (set-add used x)))]
        [else
         (for/sum ([x (in-range 0 (if (null? ls)
                                      26
                                      (car ls)))]
                   #:when (not (set-member? used x)))
           (loop (cons x ls)
                 (add1 p)
                 (set-add used x)))]))))

(define (calculate n)
  (displayln (format "Calculating n=~a:" n))
  (for/sum ([position (in-range 1 n)])
    (let ([result (calculate-pos n position)])
      (displayln (format "Position ~a: ~a" position result))
      result)))

(calculate 5)

