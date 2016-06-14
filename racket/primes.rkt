#lang racket
(require data/bit-vector)
(require "vectors.rkt")


(provide primes-up-to)
(define (primes-up-to limit)
  (let ([is-prime-vector (make-bool-sieve limit)])
    (for/list ([i (bit-vector-length is-prime-vector)]
               #:when (bit-vector-ref is-prime-vector i))
      i)))

(provide make-bool-sieve)
(define (make-bool-sieve limit)
  (let ([is-prime-vector (make-bit-vector (add1 limit) #t)])
    (bit-vector-set! is-prime-vector 0 #f)
    (bit-vector-set! is-prime-vector 1 #f)
    (for* ([prime (in-range 2 (add1 (sqrt limit)))]
           #:when (bit-vector-ref is-prime-vector prime)
           [n (in-range (* 2 prime) (add1 limit) prime)])
      (bit-vector-set! is-prime-vector n #f))
    is-prime-vector))

(provide make-totient-sieve)
(define (make-totient-sieve limit)
  (let ([is-prime-vector (make-bit-vector (add1 limit) #t)]
        [totient-vector (build-vector (add1 limit) (lambda (n) n))])
    (bit-vector-set! is-prime-vector 0 #f)
    (bit-vector-set! is-prime-vector 1 #f)

    (for ([prime (in-range 2 limit)]
          #:when (bit-vector-ref is-prime-vector prime))
      (vector-set! totient-vector prime (sub1 prime))
      (for ([n (in-range (* 2 prime) (add1 limit) prime)])
        (bit-vector-set! is-prime-vector n #f)
        (vector-set! totient-vector n (* (vector-ref totient-vector n) (- 1 (/ 1 prime))))))
    totient-vector))

(provide make-factor-sieve)
(define (make-factor-sieve limit)
  (let ([factor-vector (build-vector (add1 limit) (lambda (n) (cons n null)))])
    (for* ([prime (in-range 2 limit)]
           #:when (= 1 (length (vector-ref factor-vector prime)))
           [n (in-range (* 2 prime) (add1 limit) prime)])
      (vector-set! factor-vector n (cons prime (vector-ref factor-vector n))))
    factor-vector))

; Super slow!
(provide make-coprime-sieve)
(define (make-coprime-sieve limit)
  (let ([coprime-vector (build-vector (add1 limit) (lambda (n) (make-bit-vector n #f)))])
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

(provide sieve-prime?)
(define (sieve-prime? sieve n)
  (bit-vector-ref sieve n))

; Super slow!
(provide sieve-coprime?)
(define (sieve-coprime? sieve m n)
  (if (> n m)
      (sieve-coprime? sieve n m)
      (ormap (lambda (x) (= n x)) (vector-ref sieve m))))

(struct prime-wheel (period spokes ordered-spokes))
(define (make-prime-wheel wheel-primes)
  (define wheel-period (for/product ([p wheel-primes]) p))
  (define wheel-spokes
    (let ([sieve (make-bit-vector wheel-period #t)])
      (for* ([p wheel-primes]
             [k (in-range (/ wheel-period p))])
        (bit-vector-set! sieve (* p k) #f))
      (for/list ([i (in-range 1 wheel-period)]
                #:when (bit-vector-ref sieve i))
        i)))
  (prime-wheel wheel-period (list->set wheel-spokes) wheel-spokes))
(define wheel (make-prime-wheel '(2 3 5 7 11 13 17)))

(provide is-potential-prime?)
(define (is-potential-prime? n)
  (cond
    [(or (= n 2) (= n 3) (= n 5) (= n 7) (= n 11) (= n 13) (= n 17)) #t]
    [else (let ([wheel-mod (modulo n (prime-wheel-period wheel))])
            (set-member? (prime-wheel-spokes wheel) wheel-mod))]))

(provide is-prime?)
(define (is-prime? n)
  (cond
    [(or (= n 2) (= n 3) (= n 5) (= n 7) (= n 11) (= n 13) (= n 17)) #t]
    [else (and (is-potential-prime? n)
               (let ([sqrt-n (sqrt n)])
                 (and (not (exact? sqrt-n))
                      (for*/and ([k (in-range 0 (exact-ceiling sqrt-n) (prime-wheel-period wheel))]
                                 [m (prime-wheel-ordered-spokes wheel)]
                                 #:when (> (+ k m) 17)
                                 #:break (>= (+ k m) sqrt-n))
                        (not (zero? (modulo n (+ k m))))))))]))
