#lang racket

; wow

; (define (iterate-row row)
;   (cons 1 (append
;             (for/list ([a row] [b (cdr row)])
;               (modulo (+ a b) 7))
;             '(1))))

; (let loop ([count 0]
;            [row '(1)])
;   (displayln row)
;   (when (< count 1000)
;     (loop (add1 count) (iterate-row row))))

;-------------------------------------------------------

; (define (iterate-row row)
;   (cons 1 (append
;             (for/list ([a row] [b (cdr row)])
;               (modulo (+ a b) 7))
;             '(1))))

; (define (count-zeroes ls)
;   (for/sum ([n ls] #:when (zero? n)) 1))

; (define (test-loop limit)
;   (let loop ([row 0]
;              [ls '(1)])
;     (let ([test (f row)]
;           [actual (count-zeroes ls)])
;       (when (not (= test actual))
;         (displayln row))
;       (when (< row limit)
;         (loop (add1 row) (iterate-row ls))))))

(define (f row)
  (let loop ([q 1] [value 0])
    (let* ([7q (* 7 q)]
           [row-qt-7q-mod-7 (modulo (quotient row 7q) 7)]
           [value (+ (* (- (sub1 7q) (modulo row 7q))
                        row-qt-7q-mod-7)
                     (* value (add1 row-qt-7q-mod-7)))])
      (if (< row q)
          value
          (loop 7q value)))))

(define (calculate limit)
  (let ([display-when (expt 10 6)])
    (for/sum ([row (in-range limit)])
      (when (zero? (modulo row display-when))
        (displayln (string-append "---> " (~a row))))
      (- (add1 row) (f row)))))

(calculate (expt 10 9))
