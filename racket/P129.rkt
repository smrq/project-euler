#lang racket

(define (A n)
  (let* ([D9 (cond
               [(= (modulo n 10) 1) (* 9 n)]
               [(= (modulo n 10) 3) (* 3 n)]
               [(= (modulo n 10) 7) (* 7 n)]
               [(= (modulo n 10) 9) n]
               [else (error "n must be coprime to 10")])]
         [m (/ (add1 D9) 10)])
    (let loop ([k 1] [q 1])
      (if (zero? (modulo q n))
          k
          (loop (add1 k) (modulo (+ (* q m) 1) n))))))

(define (least-A target)
  ; A(n) < n  ==> start looking above target
  (let loop ([n0 (* (quotient target 10) 10)])
    (cond
      [(> (A (+ n0 1)) target) (+ n0 1)]
      [(> (A (+ n0 3)) target) (+ n0 3)]
      [(> (A (+ n0 7)) target) (+ n0 7)]
      [(> (A (+ n0 9)) target) (+ n0 9)]
      [else (loop (+ n0 10))])))

(least-A (expt 10 6))

; (define (repunit-divisible k D)
;   (let* ([D9 (cond
;                [(= (modulo D 10) 1) (* 9 D)]
;                [(= (modulo D 10) 3) (* 3 D)]
;                [(= (modulo D 10) 7) (* 7 D)]
;                [(= (modulo n 10) 9) n]
;                [else (error "n must be coprime to 10")])]
;          [m (/ (add1 D9) 10)])
;     (let loop ([q 1] [k-remaining (sub1 k)])
;       (if (zero? k-remaining)
;           (zero? (modulo q D))
;           (loop (+ (* q m) 1) (sub1 k-remaining))))))
