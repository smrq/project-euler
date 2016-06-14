#lang racket
(require "memo.rkt")

(provide square)
(provide cube)
(provide factorial)
(provide choose)
(provide mod+)
(provide mod-)
(provide catalan)
(provide divides?)
(provide power-of-two?)
(provide perfect-square?)
(provide fibonacci)
(provide divisors)
(provide simplex-number)

(define (square n) (* n n))
(define (cube n) (* n n n))

(define-memoized (factorial n)
   (if (<= n 1)
       1
       (* n (factorial (sub1 n)))))

(define-memoized (choose n k)
  (cond
    [(> k n) 0]
    [(= n 0) 1]
    [(= k 0) 1]
    [else
     (+ (choose (sub1 n) k)
        (choose (sub1 n) (sub1 k)))]))

(define (mod+ a b m)
  (modulo (+ a b) m))

(define (mod- a b m)
  (modulo (- (+ a m) b) m))

; https://en.wikipedia.org/wiki/Catalan_number
(define (catalan n)
  (/ (choose (* 2 n) n) (add1 n)))

(define (divides? m n)
  (zero? (modulo m n)))

(define power-of-two?
  (let* ([biggest 1]
         [results (mutable-set)]
         [add-next-power (lambda ()
                           (set! biggest (* 2 biggest))
                           (set-add! results biggest))])
    (lambda (n)
      (let loop ()
        (when (> n biggest)
          (add-next-power)
          (loop)))
      (set-member? results n))))

(define (perfect-square? n)
  (exact? (sqrt n)))

(define fibonacci
  (let ([results (make-hash)])
    (lambda (n)
      (when (not (hash-has-key? results n))
        (hash-set! results n
                   (cond
                     [(= n 1) 1]
                     [(= n 2) 1]
                     [(and (hash-has-key? results (- n 1))
                           (hash-has-key? results (- n 2)))
                      (+ (hash-ref results (- n 1))
                         (hash-ref results (- n 2)))]
                     [(power-of-two? n)
                      (let ([nn (/ n 2)])
                        (* (fibonacci nn)
                           (- (* 2 (fibonacci (add1 nn)))
                              (fibonacci nn))))]
                     [(power-of-two? (sub1 n))
                      (let ([nn (/ (sub1 n) 2)])
                        (+ (square (fibonacci nn))
                           (square (fibonacci (add1 nn)))))]
                     [else (+ (fibonacci (- n 1))
                              (fibonacci (- n 2)))])))
      (hash-ref results n))))

(define-memoized (divisors n)
  (for/list ([i (in-range 1 (add1 n))]
             #:when (divides? n i))
    i))

; dimension = 1 -> linear numbers
; dimension = 2 -> triangle numbers
; dimension = 3 -> tetrahedral numbers
; ...
(define (simplex-number n dimension)
  ; n+d-1 choose d
  (/ (for/product ([i (in-range dimension)]) (+ n i))
     (factorial dimension)))
