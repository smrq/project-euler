#lang racket
(require racket/trace)

(define limit (* 1000 1000 1000))

; A = (c/2)sqrt(ab^2 - (c/2)^2)
; `ab` must be odd, `c` must be even
(define (triangle-area-integer? ab c)
  (exact? (sqrt (- (* ab ab) (* c c 1/4)))))

(for/sum ([ab (in-range 3 (add1 (quotient (add1 limit) 3)) 2)])
  (when (zero? (modulo (sub1 ab) 10000)) (displayln ab))
  (for/sum ([c (list (add1 ab) (sub1 ab))])
    (let foo ([ab ab] [c c])
      (if (triangle-area-integer? ab c)
          (+ ab ab c)
          0))))
