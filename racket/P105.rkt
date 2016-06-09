#lang racket
(require "memo.rkt")
(require "file.rkt")

(define (sum v)
  (for/sum ([n v]) n))

(define (violates-rule-2? v)
  (let ([n (vector-length v)])
    (let loop ([more-sum (+ (vector-ref v 0) (vector-ref v 1))]
               [fewer-sum (vector-ref v (sub1 n))]
               [i 2]
               [j (- n 2)])
      (or
        (<= more-sum fewer-sum)
        (and (< i j)
             (loop
               (+ more-sum (vector-ref v i))
               (+ fewer-sum (vector-ref v j))
               (add1 i)
               (sub1 j)))))))

(define-memoized (rule-1-tests-k k)
  (let loop
    ([set1 (list 0)]
     [set2 null]
     [i 1]
     [count1 1]
     [count2 0]
     [valid? #f])
    (let ([valid? (or valid? (> count2 count1))])
      (cond
        [(and (= count1 k) (= count2 k))
         (if valid?
             (list (cons set1 set2))
             null)]
        [(= count1 k)
         (loop set1 (append set2 (list i)) (add1 i) count1 (add1 count2) valid?)]
        [(= count2 k)
         (loop (append set1 (list i)) set2 (add1 i) (add1 count1) count2 valid?)]
        [else
         (append
           (loop set1 (append set2 (list i)) (add1 i) count1 (add1 count2) valid?)
           (loop (append set1 (list i)) set2 (add1 i) (add1 count1) count2 valid?))]))))
           
(define-memoized (rule-1-tests n k)
  (let ([tests-k (rule-1-tests-k k)]
        [combos (combinations (range n) (* 2 k))])
    (for/fold
      ([acc null])
      ([test-k tests-k])
      (append acc (map (lambda (combo)
                         (cons (map (lambda (i) (list-ref combo i)) (car test-k))
                               (map (lambda (i) (list-ref combo i)) (cdr test-k))))
                       combos)))))

(define (violates-rule-1? v)
  (let ([n (vector-length v)])
    (for/or ([k (in-range 2 (add1 (quotient n 2)))])
      (for/or ([test (rule-1-tests n k)])
        (= (for/sum ([i (car test)]) (vector-ref v i))
           (for/sum ([i (cdr test)]) (vector-ref v i)))))))

(define sets
  (parse-file "P105_sets.txt"
              (lambda (str)
                (list->vector
                  (sort
                    (map string->number (string-split str ","))
                    <)))))

(for/sum ([v sets])
  (if (or (violates-rule-2? v) (violates-rule-1? v))
      0
      (sum v)))
