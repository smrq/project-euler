#lang racket
(require "digits.rkt")
(require "math.rkt")
(require "memo.rkt")

(define-memoized (iterate n)
  (for/sum ([i (map factorial (digits n))]) i))

(define-memoized (loop-length n)
  (let ([hash (make-hash)])
    (let recur ([n n]
                [times 0])
      (if (hash-has-key? hash n)
          times
          (begin
            (hash-set! hash n times)
            (recur (iterate n) (add1 times)))))))

(define loop-lengths
  (map (lambda (n)
         (loop-length n))
       (build-list 1000000 values)))

(length (filter (lambda (x) (= 60 x)) loop-lengths))
