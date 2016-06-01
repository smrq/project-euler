#lang racket
(require csv-reading)

(define raw-data
  (let ([result null])
    (define in (open-input-file "P82_matrix.txt"))
    (set! result (csv->list in))
    (close-input-port in)
    result))

(define matrix
  (list->vector (map (lambda (row) (list->vector (map string->number row)))
                     raw-data)))
(define matrix-size (vector-length matrix))

(define (matrix-set! matrix row col value)
  (vector-set! (vector-ref matrix row) col value))

(define (matrix-ref matrix row col)
  (vector-ref (vector-ref matrix row) col))

(define cost-matrix
  (build-vector matrix-size (lambda (_) (make-vector matrix-size +inf.0))))
(define visited-matrix
  (build-vector matrix-size (lambda (_) (make-vector matrix-size #f))))
(define end-cost +inf.0)


; https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
(define (neighbors-of node)
  (let* ([row (car node)]
         [col (cdr node)]
         [with-right (if (= (add1 col) matrix-size)
                         (cons 'end null)
                         (cons (cons row (add1 col)) null))]
         [with-top (if (or (zero? row) (matrix-ref visited-matrix (sub1 row) col))
                       with-right
                       (cons (cons (sub1 row) col) with-right))]
         [with-bottom (if (or (= (add1 row) matrix-size) (matrix-ref visited-matrix (add1 row) col))
                          with-top
                          (cons (cons (add1 row) col) with-top))])
    with-bottom))

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
    (cons result lowest-cost)))

(let loop ([current 'start]
      [cost 0]
      [neighbors (build-list matrix-size (lambda (row) (cons row 0)))])
  (for/list ([neighbor neighbors])
    (cons neighbor
          (cond
            [(eq? neighbor 'end)
             (set! end-cost (min cost end-cost))
             end-cost]
            [else
             (let* ([row (car neighbor)]
                    [col (cdr neighbor)]
                    [cell-cost (matrix-ref matrix row col)])
               (matrix-set! cost-matrix row col
                            (min (+ cost cell-cost)
                                 (matrix-ref cost-matrix row col)))
               (matrix-ref cost-matrix row col))])))
  (unless (eq? current 'start)
    (matrix-set! visited-matrix (car current) (cdr current) #t))
  (let ([next-node (select-next-node)])
    (cond
      [(eq? (car next-node) 'end)
       (cdr next-node)]
      [else
       (loop
         (car next-node)
         (cdr next-node)
         (neighbors-of (car next-node)))])))
