#lang racket

; Ag(x) = x(G1 + Ag(x)) + x^2(G2 - G1 + Ag(x))
;     0 = x^2(3 + Ag(x)) + x(1 + Ag(x)) - Ag(x)
;     x = (-n - 1 + sqrt(5n^2 + 14n + 1)) / (2n + 6)
; x is rational iff (5n^2 + 14n + 1) is a perfect square

; (define (perfect-square? n)
;   (exact? (sqrt n)))
; (let loop ([count 0] [n 1])
;   (if (perfect-square? (+ (* 5 n n) (* 14 n) 1))
;       (let ([count (add1 count)])
;         (displayln (string-append (~a count) ": " (~a n)))
;         (if (= count 30)
;             n
;             (loop count (add1 n))))
;       (loop count (add1 n))))

; 1: 2
; 2: 5
; 3: 21
; 4: 42
; 5: 152
; 6: 296
; 7: 1050
; 8: 2037
; 9: 7205
; 10: 13970
; 11: 49392
; 12: 95760
; 13: 338546
; 14: 656357
; 15: 2320437
; 16: 4498746
; 17: 15904520
; 18: 30834872
; 19: 109011210
; 20: 211345365

; ---------------------------------------

; (define ns '(2 5 21 42 152 296 1050 2037 7205 13970 49392 95760 338546 656357 2320437 4498746 15904520 30834872 109011210 211345365))

; (require "memo.rkt")
; (require "math.rkt")
; (define (G n)
;   (cond [(= n 1) 1]
;         [(= n 2) 4]
;         [else (+ (G (- n 1)) (G (- n 2)))]))

; (define gs (map (lambda (n) (* (G n) (G (add1 n)))) (range 1 25)))
; (define fs (map fibonacci (range 1 25)))

; (for/list ([a ns] [b (cddr ns)]) (exact->inexact (/ b a)))
; (for/list ([a gs] [b (cddr gs)]) (exact->inexact (/ b a)))
; (for/list ([a fs] [b (cddddr fs)]) (exact->inexact (/ b a)))

; WHAT DOES IT MEAN???

; ---------------------------------------

; No idea how this pattern is derived, but it's kind of like
; the one from P137

(require "memo.rkt")
(define-memoized (N n)
  (cond [(= n 1) 2]
        [(= n 2) 5]
        [(= n 3) 21]
        [(= n 4) 42]
        [else (+ (* 7 (N (- n 2)))
                 (- (N (- n 4)))
                 7)]))

(for/sum ([i (in-range 1 (add1 30))])
  (N i))
