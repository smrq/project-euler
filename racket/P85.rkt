#lang racket

(define (count-rectangles w h)
  (/ (* w h (add1 w) (add1 h)) 4))

; The rest of this file has been lost! :(
