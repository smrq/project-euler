#lang racket
(require data/bit-vector)

(provide primes-up-to)

; Eratosthenes sieve method
(define (primes-up-to limit)
  (define is-prime-vector (make-bit-vector (add1 limit) #t))
  (bit-vector-set! is-prime-vector 0 #f)
  (bit-vector-set! is-prime-vector 1 #f)
  (for* ([prime (in-range 2 (add1 (sqrt limit)))] #:when (bit-vector-ref is-prime-vector prime)
         [n (in-range (* 2 prime) (add1 limit) prime)])
    (bit-vector-set! is-prime-vector n #f))
  (for/list ([i (bit-vector-length is-prime-vector)] #:when (bit-vector-ref is-prime-vector i))
    i))
