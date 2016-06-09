#lang racket

(provide parse-file)

(define (parse-file filename parse-line)
  (call-with-input-file filename
    (lambda (in)
      (let loop ([acc null])
        (let ([str (read-line in)])
          (if (eof-object? str)
              acc
              (loop (cons (parse-line str) acc))))))))
