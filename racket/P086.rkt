#lang racket
(require "pythagorean-triples.rkt")

(define (count-triple-decompositions triple M)
  (let ([a (first triple)] [b (second triple)])
    (+ (if (<= b M)
           (quotient a 2) ; A = b, B1 = ceil(a/2)...a-1, B2 = floor(a/2)...1
           0)
       (if (<= (/ b 2) a)
           (- (add1 (quotient b 2)) (- b a)) ; A = a, B1 = a...ceil(b/2), B2 = b-a...floor(b/2)
           0))))

(define (is-within-limit M triple)
  (and
    (<= (first triple) M)
    (not (zero? (count-triple-decompositions triple M)))))

(define target 1000000)

(let loop-M ([M 100])
  (define solutions 0)
  (let loop-triples ([triples base-triples])
    (if (not (empty? triples))
        (begin
          (for ([triple triples])
            (let loop-k ([k 1])
              (let foo ([triple-k (map (lambda (x) (* x k)) triple)])
                (when (is-within-limit M triple-k)
                  (set! solutions (+ solutions (count-triple-decompositions triple-k M)))
                  (loop-k (add1 k))))))
          (let ([next-triples
                 (filter (lambda (t) (is-within-limit M t)) (generate-next-triples triples))])
            (loop-triples next-triples)))
        (if (< solutions target)
            (loop-M (add1 M))
            (cons M solutions)))))
