#lang racket
(require "primes.rkt")

(define limit (* 150 (expt 10 6)))

(require racket/serialize)
(define (generate-primes)
  (displayln (string-append "Generating primes up to " (~a limit) "..."))
  (define primes (primes-up-to limit))
  (call-with-output-file "primes.txt"
    (lambda (out)
      (write (serialize primes) out)))
  (displayln "Done! Wrote primes to primes.txt.")
  primes)

(define (load-primes)
  (displayln "Loading primes from primes.txt...")
  (define primes (call-with-input-file "primes.txt"
                   (lambda (in)
                     (deserialize (read in)))))
  (displayln "Done!")
  primes)

; (define primes (generate-primes))
(define primes (load-primes))

(define (nsq-plus-k-divisible-by-p? nsq k p nsq-mod-p)
  (= nsq-mod-p (modulo (- p k) p)))

(define (f? n)
  (let* ([nsq (sqr n)]
         [p-limit (sqrt (+ nsq 27))])
    (and
      (for/and ([p primes] #:break (> p p-limit))
        (let ([mod (modulo nsq p)])
          (for/and ([k '(1 3 7 9 13 27)])
            (not (nsq-plus-k-divisible-by-p? nsq k p mod)))))
      (for/and ([k '(5 11 15 17 19 21 23 25)])
        (for/or ([p primes] #:break (> p p-limit))
          (let ([mod (modulo nsq p)])
            (nsq-plus-k-divisible-by-p? nsq k p mod)))))))

(displayln "Calculating prime patterns...")
(for/sum ([n (in-range 10 limit 2)]
          #:when (f? n))
  n)
