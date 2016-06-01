#lang racket

(require racket/trace)

(provide make-matrix)
(provide build-matrix)
(provide matrix-set!)
(provide matrix-inc!)
(provide matrix-dec!)
(provide matrix-ref)
(provide matrix-row-length)
(provide matrix-col-length)
(provide show-matrix)
(provide gaussian-eigenvector!)

(define (make-matrix rows cols value)
  (build-matrix rows cols (lambda (r c) value)))
(define (build-matrix rows cols fn)
  (build-vector rows
                (lambda (row)
                  (build-vector cols
                                (lambda (col) (fn row col))))))
(define (matrix-ref matrix row col)
  (vector-ref (vector-ref matrix row) col))
(define (matrix-set! matrix row col value)
  (vector-set! (vector-ref matrix row) col value))
(define (matrix-inc! matrix row col value)
  (matrix-set! matrix row col (+ (matrix-ref matrix row col) value)))
(define (matrix-dec! matrix row col value)
  (matrix-set! matrix row col (- (matrix-ref matrix row col) value)))
(define (matrix-row-length matrix)
  (vector-length matrix))
(define (matrix-col-length matrix)
  (vector-length (vector-ref matrix 0)))

(define (show-matrix matrix)
  (for ([i (in-range (matrix-row-length matrix))])
    (for ([j (in-range (matrix-col-length matrix))])
      (display (~r (matrix-ref matrix i j) #:precision '(= 3)))
      (display ", "))
    (displayln "")))

(define (vector-dec! vector i value)
  (vector-set! vector i (- (vector-ref vector i) value)))
(define (gaussian-eigenvector! matrix eigenvalue)
  ; Solve Ax = vx for x
  ; This destroys the matrix!
  (let* ([matrix-size (matrix-row-length matrix)]
         [swap-rows! (lambda (row1 row2)
                       (let ([origRow1 (vector-ref matrix row1)]
                             [origRow2 (vector-ref matrix row2)])
                         (vector-set! matrix row1 origRow2)
                         (vector-set! matrix row2 origRow1)))])

    ; (A-vI)x = 0
    (for ([i (in-range matrix-size)])
      (matrix-dec! matrix i i eigenvalue))

    ; https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode
    (for ([k (in-range matrix-size)])
      (for ([i (in-range (add1 k) matrix-size)])
        (let ([f (/ (matrix-ref matrix i k) (matrix-ref matrix k k))])
          (for ([j (in-range (add1 k) matrix-size)])
            (matrix-dec! matrix i j (* f (matrix-ref matrix k j))))
          (matrix-set! matrix i k 0))))

    ; Backpropagation
    (let ([v (make-vector matrix-size)])
      (vector-set! v (sub1 matrix-size) 1)
      (for ([i (in-range (- matrix-size 2) -1 -1)])
        (vector-set! v i
                     (/
                       (for/sum ([j (in-range (add1 i) matrix-size)])
                         (* (vector-ref v j) (matrix-ref matrix i j)))
                       (- (matrix-ref matrix i i)))))
      v)))
