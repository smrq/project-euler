#lang racket
(require data/bit-vector)

(provide primes-up-to)
(provide make-totient-sieve)

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
        [totient-vector (make-vector (add1 limit) 0)])
    (bit-vector-set! is-prime-vector 0 #f)
    (bit-vector-set! is-prime-vector 1 #f)
    (for ([n (in-range (vector-length totient-vector))])
      (vector-set! totient-vector n n))

    (for ([prime (in-range 2 limit)]
          #:when (bit-vector-ref is-prime-vector prime))
      (vector-set! totient-vector prime (sub1 prime))
      (for ([n (in-range (* 2 prime) (add1 limit) prime)]
            #:break (> n (vector-length totient-vector)))
        (bit-vector-set! is-prime-vector n #f)
        (vector-set! totient-vector n (* (vector-ref totient-vector n) (- 1 (/ 1 prime))))))
    totient-vector))
