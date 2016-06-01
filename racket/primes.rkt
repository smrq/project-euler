#lang racket
(require data/bit-vector)
(require "vectors.rkt")

(provide primes-up-to)
(provide make-bool-sieve)
(provide make-factor-sieve)
(provide make-totient-sieve)
(provide make-coprime-sieve)
(provide sieve-prime?)
(provide sieve-coprime?)

(define (primes-up-to limit)
  (let ([is-prime-vector (make-bool-sieve limit)])
    (for/list ([i (bit-vector-length is-prime-vector)]
               #:when (bit-vector-ref is-prime-vector i))
      i)))

(define (make-bool-sieve limit)
  (let ([is-prime-vector (make-bit-vector (add1 limit) #t)])
    (bit-vector-set! is-prime-vector 0 #f)
    (bit-vector-set! is-prime-vector 1 #f)
    (for* ([prime (in-range 2 (add1 (sqrt limit)))]
           #:when (bit-vector-ref is-prime-vector prime)
           [n (in-range (* 2 prime) (add1 limit) prime)])
      (bit-vector-set! is-prime-vector n #f))
    is-prime-vector))

(define (make-totient-sieve limit)
  (let ([is-prime-vector (make-bit-vector (add1 limit) #t)]
        [totient-vector (make-vector-by-index (add1 limit) (lambda (n) n))])
    (bit-vector-set! is-prime-vector 0 #f)
    (bit-vector-set! is-prime-vector 1 #f)

    (for ([prime (in-range 2 limit)]
          #:when (bit-vector-ref is-prime-vector prime))
      (vector-set! totient-vector prime (sub1 prime))
      (for ([n (in-range (* 2 prime) (add1 limit) prime)])
        (bit-vector-set! is-prime-vector n #f)
        (vector-set! totient-vector n (* (vector-ref totient-vector n) (- 1 (/ 1 prime))))))
    totient-vector))

(define (make-factor-sieve limit)
  (let ([factor-vector (make-vector-by-index (add1 limit) (lambda (n) (cons n null)))])
    (for* ([prime (in-range 2 limit)]
           #:when (= 1 (length (vector-ref factor-vector prime)))
           [n (in-range (* 2 prime) (add1 limit) prime)])
      (vector-set! factor-vector n (cons prime (vector-ref factor-vector n))))
    factor-vector))

; Super slow!
(define (make-coprime-sieve limit)
  (let ([coprime-vector (make-vector-by-index (add1 limit) (lambda (n) (make-bit-vector n #f)))])
    (for ([prime (in-range 2 limit)]
          #:when (bit-vector-none? (vector-ref coprime-vector prime)))
      (for* ([n (in-range (* 2 prime) (add1 limit) prime)]
             [m (in-range prime n prime)])
        (bit-vector-set! (vector-ref coprime-vector n) m #t)))

    (vector-map
      (lambda (v)
        (for/list ([i (in-range 1 (bit-vector-length v))]
                   #:unless (bit-vector-ref v i))
          i))
      coprime-vector)))

(define (sieve-prime? sieve n)
  (bit-vector-ref sieve n))

; Super slow!
(define (sieve-coprime? sieve m n)
  (if (> n m)
      (sieve-coprime? sieve n m)
      (ormap (lambda (x) (= n x)) (vector-ref sieve m))))
