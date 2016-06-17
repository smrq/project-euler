#lang racket

; https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples

(provide base-triples)
(define base-triples (list '(3 4 5)))

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
