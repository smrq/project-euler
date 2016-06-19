#lang racket

; x + y = a^2
; x - y = b^2
; x + z = c^2
; x - z = d^2
; y + z = e^2
; y - z = f^2

(define search-limit 1000)

(for* ([d (in-range 1 search-limit)]
       [c (in-range (+ 2 d) search-limit 2)])
  (let* ([csq (sqr c)]
         [dsq (sqr d)]
         [2z (- csq dsq)])
    (for* ([f (in-range 1 d)])
      (let* ([fsq (sqr f)]
             [esq (+ fsq 2z)]
             [e (sqrt esq)])
        (when (exact? e)
          (let* ([asq (+ dsq fsq 2z)]
                 [bsq (- dsq fsq)])
            (when (and (exact? (sqrt asq))
                       (exact? (sqrt bsq)))
              (let* ([z (/ 2z 2)] [x (+ dsq z)] [y (+ fsq z)])
                (displayln (+ x y z))))))))))
