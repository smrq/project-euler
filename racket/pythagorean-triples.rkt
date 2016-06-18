#lang racket

; https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples

(provide base-triple)
(define base-triple '(3 4 5))

(provide base-triples)
(define base-triples (list base-triple))

(provide generate-next-triples)
(define (generate-next-triples triples)
  (apply append (map generate-triples-from-seed triples)))

(define (generate-triples-from-seed triple)
  (list (next-triple-a triple)
        (next-triple-b triple)
        (next-triple-c triple)))

(provide next-triple-a)
(define (next-triple-a triple)
  (let ([x (first triple)] [y (second triple)] [z (third triple)])
    (sort
      (list
        (+ (* x 1) (* y -2) (* z 2))
        (+ (* x 2) (* y -1) (* z 2))
        (+ (* x 2) (* y -2) (* z 3)))
      <)))

(provide next-triple-b)
(define (next-triple-b triple)
  (let ([x (first triple)] [y (second triple)] [z (third triple)])
    (sort
      (list
        (+ (* x 1) (* y 2) (* z 2))
        (+ (* x 2) (* y 1) (* z 2))
        (+ (* x 2) (* y 2) (* z 3)))
      <)))

(provide next-triple-c)
(define (next-triple-c triple)
  (let ([x (first triple)] [y (second triple)] [z (third triple)])
    (sort
      (list
        (+ (* x -1) (* y 2) (* z 2))
        (+ (* x -2) (* y 1) (* z 2))
        (+ (* x -2) (* y 2) (* z 3)))
      <)))

(provide triple-find-pattern)
(define (triple-find-pattern fn search-depth max-k)
  (let loop ([triple (first base-triples)] [level 0] [path "x"])
    (for ([k (in-range 1 (add1 max-k))])
      (when (fn (first triple) (second triple) (third triple) k)
        (displayln (string-append "k=" (~a k) ": " path))))
    (when (< level search-depth)
      (loop (next-triple-a triple) (add1 level) (string-append "A" path))
      (loop (next-triple-b triple) (add1 level) (string-append "B" path))
      (loop (next-triple-c triple) (add1 level) (string-append "C" path)))))
