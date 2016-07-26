#lang racket

(define limit 18)

(define (combine-parallel s1 s2)
  (for*/set ([a s1]
             [b s2])
    (+ a b)))

(define (combine-serial s1 s2)
  (for*/set ([a s1]
             [b s2])
    (/ 1 (+ (/ 1 a)
            (/ 1 b)))))

(define (combine s1 s2)
  (set-union (combine-parallel s1 s2)
             (combine-serial s1 s2)))

(define solution-v (make-vector (add1 limit)))
(vector-set! solution-v 0 (set))
(vector-set! solution-v 1 (set 1))

(for ([d (in-range 2 (add1 limit))])
  (displayln (format "Calculating ~a..." d))
  (vector-set! solution-v d
               (apply set-union
                      (for/list ([k (in-range 1 (add1 (quotient d 2)))])
                        (combine (vector-ref solution-v k)
                                 (vector-ref solution-v (- d k)))))))
(displayln "Calculating final set...")
(set-count (apply set-union (for/list ([s solution-v]) s)))
