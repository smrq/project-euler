#lang racket
(require csv-reading)

(define raw-data
  (let ([result null])
    (define in (open-input-file "P81_matrix.txt"))
    (set! result (csv->list in))
    (close-input-port in)
    result))

(define matrix
  (list->vector (map (lambda (row) (list->vector (map string->number row)))
                     raw-data)))

(define (matrix-set! matrix i j value)
  (vector-set! (vector-ref matrix i) j value))

(define (matrix-ref matrix i j)
  (vector-ref (vector-ref matrix i) j))

; Transform into cost matrix
(for ([i (in-range (vector-length matrix))])
  (for ([j (in-range (vector-length (vector-ref matrix i)))])
    (cond
      [(and (zero? i) (zero? j)) #f]
      [(zero? i) (matrix-set! matrix i j (+ (matrix-ref matrix i j) (matrix-ref matrix i (sub1 j))))]
      [(zero? j) (matrix-set! matrix i j (+ (matrix-ref matrix i j) (matrix-ref matrix (sub1 i) j)))]
      [else (matrix-set! matrix i j (+ (matrix-ref matrix i j)
                                       (min
                                         (matrix-ref matrix (sub1 i) j)
                                         (matrix-ref matrix i (sub1 j)))))])))

(matrix-ref matrix 79 79)
