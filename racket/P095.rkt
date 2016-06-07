#lang racket
(require "memo.rkt")
(require racket/trace)

(define limit (add1 1000000))
(define amicable-vector (make-vector limit #f))

(define (sum ls)
  (for/sum ([n ls]) n))

(define (proper-divisors n)
  (let ([max (add1 (integer-sqrt n))])
    (cons 1
      (let loop ([d 2] [acc null])
        (if (>= d max) acc
            (let-values ([(q r) (quotient/remainder n d)])
              (cond [(not (zero? r)) (loop (add1 d) acc)]
                    [(= q d) (loop (add1 d) (cons d acc))]
                    [else (loop (add1 d) (cons q (cons d acc)))])))))))

(define (amicable-iterate n)
  (sum (proper-divisors n)))

(define (calculate-chain n)
  (when (not (vector-ref amicable-vector n))
    (let loop ([i n] [ls (list n)])
      (let a ([a (amicable-iterate i)])
        (cond
          [(>= a limit)
           (for ([j ls])
             (when (not (vector-ref amicable-vector j))
               (vector-set! amicable-vector j 0)))]
          [(vector-ref amicable-vector i)
           (for ([j ls])
             (when (not (vector-ref amicable-vector j))
               (vector-set! amicable-vector j 0)))]
          [else
           (let-values ([(in-chain out-chain) (splitf-at-right ls (lambda (x) (not (= x a))))])
             (if (null? in-chain)
                 (loop a (cons a ls))
                 (let ([chain-length (length in-chain)])
                   (vector-set! amicable-vector i chain-length)
                   (for ([j in-chain])
                     (vector-set! amicable-vector j chain-length)
                   (for ([j out-chain])
                     (when (not (vector-ref amicable-vector j))
                       (vector-set! amicable-vector j 0)))))))])))))

(for ([i (in-range 1 limit)])
  (when (zero? (modulo i 10000))
      (displayln i))
  (calculate-chain i))

(vector-member (vector-argmax (lambda (x) (or x -1)) amicable-vector) amicable-vector)
