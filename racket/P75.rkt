#lang racket

(define (euclid-triple m n k)
  (let ([a (* k (- (* m m) (* n n)))]
        [b (* k 2 m n)]
        [c (* k (+ (* m m) (* n n)))])
    (sort `(,a ,b ,c) <)))

(define (triple-perimeter triple)
  (apply + triple))

(define limit 1500000)

(define triple-counts
  (let ([counts (vector-map (lambda (x) (make-hash)) (make-vector (add1 limit) 0))])
    (let loop-m ([m 2])
      (let loop-n ([n 1])
        (cond
          [(>= n m) (loop-m (add1 m))]
          [(even? (- m n)) (loop-n (add1 n))]
          ; [(not (coprime? (- m n)) (loop-n (add1 n))] ; Too slow! Use a hash to reject duplicate triples instead
          [else
            (let loop-k ([k 1])
              (let* ([triple (euclid-triple m n k)]
                     [perim (triple-perimeter triple)])
                (if (> perim limit)
                    (if (and (= n 1) (= k 1))
                        (vector-map hash-count counts)
                        (loop-n (add1 n)))
                    (begin
                      (hash-set! (vector-ref counts perim) triple 1)
                      (loop-k (add1 k))))))])))))

(vector-count (lambda (x) (= x 1)) triple-counts)
