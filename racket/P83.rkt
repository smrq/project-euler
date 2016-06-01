#lang racket
(require csv-reading)
(require "matrix.rkt")

(define raw-data
  (let ([result null])
    (define in (open-input-file "P83_matrix.txt"))
    (set! result (csv->list in))
    (close-input-port in)
    result))

(define matrix
  (list->vector (map (lambda (row) (list->vector (map string->number row)))
                     raw-data)))
(define matrix-size (vector-length matrix))

(define cost-matrix (make-matrix matrix-size matrix-size +inf.0))
(define visited-matrix (make-matrix matrix-size matrix-size #f))
(define end-cost +inf.0)

; https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
(define (dijkstra get-neighbors get-cost get-tentative-cost set-tentative-cost! get-visited set-visited! select-next-node)
  (let loop ([current 'start] [cost 0])
    (let ([neighbors (get-neighbors current)])
      (for ([neighbor neighbors] #:when (not (get-visited neighbor)))
        (set-tentative-cost! neighbor (min (+ cost (get-cost current neighbor)) (get-tentative-cost neighbor))))
      (set-visited! current)
      (let* ([next-node (select-next-node)]
             [next-cost (get-tentative-cost next-node)])
        (cond
          [(eq? next-node 'end) next-cost]
          [else (loop next-node next-cost)])))))

(define (get-neighbors node)
  (cond
    [(eq? node 'start) (list (cons 0 0))]
    [else
     (let* ([row (car node)]
            [col (cdr node)]
            [neighbors null]
            [neighbors (if (and (= (add1 row) matrix-size)
                                (= (add1 col) matrix-size))
                           (cons 'end neighbors)
                           neighbors)]
            [neighbors (if (or (zero? col) (matrix-ref visited-matrix row (sub1 col)))
                           neighbors
                           (cons (cons row (sub1 col)) neighbors))]
            [neighbors (if (or (= (add1 col) matrix-size) (matrix-ref visited-matrix row (add1 col)))
                           neighbors
                           (cons (cons row (add1 col)) neighbors))]
            [neighbors (if (or (zero? row) (matrix-ref visited-matrix (sub1 row) col))
                           neighbors
                           (cons (cons (sub1 row) col) neighbors))]
            [neighbors (if (or (= (add1 row) matrix-size) (matrix-ref visited-matrix (add1 row) col))
                           neighbors
                           (cons (cons (add1 row) col) neighbors))])
       neighbors)]))
(define (get-cost _ node)
  (cond
    [(eq? node 'end) 0]
    [else (matrix-ref matrix (car node) (cdr node))]))
(define (get-tentative-cost node)
  (cond
    [(eq? node 'end) end-cost]
    [else (matrix-ref cost-matrix (car node) (cdr node))]))
(define (set-tentative-cost! node cost)
  (cond
    [(eq? node 'end) (set! end-cost cost)]
    [else (matrix-set! cost-matrix (car node) (cdr node) cost)]))
(define (get-visited node)
  (and (not (eq? node 'end))
       (matrix-ref visited-matrix (car node) (cdr node))))
(define (set-visited! node)
  (when (not (eq? node 'start))
    (matrix-set! visited-matrix (car node) (cdr node) #t)))
(define (select-next-node)
  (let ([result 'end]
        [lowest-cost end-cost])
    (for* ([row (in-range matrix-size)]
           [col (in-range matrix-size)])
      (let* ([ijvisited (matrix-ref visited-matrix row col)]
             [ijcost (matrix-ref cost-matrix row col)])
        (when (and (not ijvisited)
                   (< ijcost lowest-cost))
          (set! result (cons row col))
          (set! lowest-cost ijcost))))
    result))

(dijkstra get-neighbors get-cost get-tentative-cost set-tentative-cost! get-visited set-visited! select-next-node)
