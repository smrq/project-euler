#lang racket

(define (vec a b)
  (cons
    (- (car b) (car a))
    (- (cdr b) (cdr a))))

(define (dot a b)
  (+ (* (car a) (car b))
     (* (cdr a) (cdr b))))

(define (in-triangle a b c p)
  ; Use ab and ac as basis vectors, then determine if p is within the triangle (0,0) (1,0) (0,1) in uv-space
  (let* ([ab (vec a b)]
         [ac (vec a c)]
         [ap (vec a p)]
         [magnitude-ab (dot ab ab)]
         [magnitude-ac (dot ac ac)]
         [ac-on-ab (dot ac ab)]
         [ap-on-ab (dot ab ap)]
         [ap-on-ac (dot ac ap)]
         [u (/ (- (* magnitude-ab ap-on-ac) (* ac-on-ab ap-on-ab))
               (- (* magnitude-ab magnitude-ac) (* ac-on-ab ac-on-ab)))]
         [v (/ (- (* magnitude-ac ap-on-ab) (* ac-on-ab ap-on-ac))
               (- (* magnitude-ab magnitude-ac) (* ac-on-ab ac-on-ab)))])
    (and (> u 0)
         (> v 0)
         (< (+ u v) 1))))

(define (parse-line str)
  (map string->number (string-split str ",")))

(define triangles
  (call-with-input-file "P102_triangles.txt"
    (lambda (in)
      (let loop ([acc null])
        (let ([str (read-line in)])
          (if (eof-object? str)
              acc
              (loop (cons (parse-line str) acc))))))))

(for/sum ([triangle triangles])
  (if (in-triangle
        (cons (first triangle) (second triangle))
        (cons (third triangle) (fourth triangle))
        (cons (fifth triangle) (sixth triangle))
        (cons 0 0))
      1
      0))
