#lang racket
(require "memo.rkt")
(require "digits.rkt")
(require "primes.rkt")

(define-memoized (try-perm p acc ps)
  (if (null? p)
    (if (zero? acc)
        (/ 1 (length (permutations ps)))
        0)
    (let ([acc (+ (car p) (* 10 acc))])
      (cond
        [(is-prime? acc)
         (+ (try-perm (cdr p) 0 (cons acc ps))
            (try-perm (cdr p) acc ps))]
        [else (try-perm (cdr p) acc ps)]))))

(define perms (permutations (range 1 10)))

(for/sum ([p perms]
          [n (length perms)])
  (when (zero? (modulo n 1000)) (displayln (string-append (~a n) " / " (~a (length perms)))))
  (try-perm p 0 null))
