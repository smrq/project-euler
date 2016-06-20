#lang racket

; First brute force attempt
; based on: t^4 + a^4 + b^4 + c^4 = t^2(a^2 + b^2 + c^2) + a^2(b^2 + c^2) + b^2(c^2)

; (define search-limit 1000)

; (for* ([a (in-range 1 search-limit)]
;        [b (in-range a search-limit)]
;        [c (in-range b search-limit)]
;        [t (in-range c search-limit)])
;   (let ([asq (sqr a)]
;         [bsq (sqr b)]
;         [csq (sqr c)])
;     (when (> (* a b) (- csq asq bsq)) ; Largest angle < 120deg
;       (let* ([tsq (sqr t)]
;              [thy (sqr tsq)]
;              [ahy (sqr asq)]
;              [bhy (sqr bsq)]
;              [chy (sqr csq)])
;         (when (= (sqr (+ tsq asq bsq csq))
;                  (* 3 (+ thy ahy bhy chy)))
;           (displayln (string-append (~a t) ": "
;                                     "(" (~a a) "," (~a b) "," (~a c) "): "
;                                     "3(" (~a thy) "+" (~a ahy) "+" (~a bhy) "+" (~a chy) ")=(" (~a tsq) "+" (~a asq) "+" (~a bsq) "+" (~a csq) ")^2")))))))

; (define (t a b c)
;   (sqrt (/
;           (+ (* a a)
;              (* b b)
;              (* c c)
;              (sqrt (- (* 6 (+ (* a a b b)
;                               (* a a c c)
;                               (* b b c c)))
;                       (* 3 (+ (* a a a a)
;                               (* b b b b)
;                               (* c c c c))))))
;           2)))

;------------------------------------------

; a^2 = q^2 + 2qr + r^2
; b^2 = p^2 + 2pr + r^2
; c^2 = p^2 + 2pq + q^2

; (define limit 10000)
(define limit 120000)

(displayln "Generating squares...")
(define perfect-squares
  (for/set ([n (in-naturals 1)]
            #:break (> n limit))
    (sqr n)))
(define (perfect-square? n)
  (set-member? perfect-squares n))


; For each triangle TAB, TBC, TCA:
; cos(T) = p^2 + q^2 - a^2 / 2pq
; Angle T is always 120deg => cos(T) = -1/2
; a^2 = p^2 + pq + q^2
;     = (p+q)^2 - pq
; pq  = (p+q)^2 - a^2

(displayln "Generating pairs p,q...")
; Brute force
(define square-dict (make-hash))
(for* ([p (in-range 1 limit)]
       [q (in-range p (- limit p))]
       #:when (perfect-square? (- (sqr (+ p q)) (* p q))))
  (dict-set! square-dict p
             (set-add (dict-ref! square-dict p (set)) q)))

; Find triples p,q,r where p,q; p,r; q,r all valid pairs

(displayln "Generating triples p,q,r...")
(define pqrs
  (for*/set ([x (in-dict-keys square-dict)]
             [y (in-set (dict-ref square-dict x))]
             #:when (> y x)
             [z (in-set (set-intersect (dict-ref square-dict x) (dict-ref square-dict y (set))))]
             #:when (and (> z y)
                         (< (+ x y z) limit)))
    (+ x y z)))
(for/sum ([pqr (in-set pqrs)]) pqr)
