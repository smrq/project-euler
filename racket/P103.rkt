#lang racket
(require "memo.rkt")

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

(define (merge-candidates c1 c2)
  (let loop ([c1 c1]
             [c2 c2])
    (cond
      [(and (null? c1) (null? c2)) null]
      [(null? c1) c2]
      [(null? c2) c1]
      [(< (caar c1) (caar c2))
       (cons (car c1) (loop (cdr c1) c2))]
      [(> (caar c1) (caar c2))
       (cons (car c2) (loop c1 (cdr c2)))]
      [else
       (cons
         (cons (caar c1)
               (set-union (cdar c1) (cdar c2)))
         (loop (cdr c1) (cdr c2)))])))

(define (increment-rule-1 candidate)
  (let ([next-cost (add1 (sum candidate))])
    (list (cons next-cost
                (for/set ([i (in-range (vector-length candidate))]
                           #:when (or (= i (sub1 (vector-length candidate)))
                                      (> (- (vector-ref candidate (add1 i))
                                            (vector-ref candidate i))
                                         1)))
                  (let ([new-v (vector-copy candidate)])
                    (vector-set! new-v i (add1 (vector-ref new-v i)))
                    new-v))))))

(define (increment-rule-2 candidate)
  (let loop ([acc null]
             [i 0])
    (cond
      [(> i (/ (vector-length candidate) 2)) acc]
      [else
       (let ([new-v (vector-copy candidate)])
         (vector-set! new-v i (add1 (vector-ref new-v i)))
         (for ([j (in-range (add1 i) (vector-length new-v))]
               #:break (> (vector-ref new-v j) (vector-ref new-v (sub1 j))))
           (vector-set! new-v j (add1 (vector-ref new-v j))))
         (loop 
           (merge-candidates acc (list (cons (sum new-v) (set new-v))))
           (add1 i)))])))

(define (iterate-candidates candidates)
  (let loop ([current-candidates (cdar candidates)]
             [candidates (cdr candidates)]
             [next-candidates null])
    (if (set-empty? current-candidates)
        (iterate-candidates (merge-candidates candidates next-candidates))
        (let ([candidate (set-first current-candidates)]
              [current-candidates (set-rest current-candidates)])
          (cond
            [(violates-rule-2? candidate)
             (loop current-candidates
                   candidates
                   (merge-candidates next-candidates (increment-rule-2 candidate)))]
            [(violates-rule-1? candidate)
             (loop current-candidates
                   candidates
                   (merge-candidates next-candidates (increment-rule-1 candidate)))]
            [else candidate])))))

(define (optimal n)
  (iterate-candidates (list (cons #f (set (build-vector n add1))))))

(optimal 7)
