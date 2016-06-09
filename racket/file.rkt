#lang racket

(provide parse-file)

(define (parse-file filename parse-line)
  (call-with-input-file filename
    (lambda (in)
      (let loop ()
        (let ([str (read-line in)])
          (if (eof-object? str)
              null
              (cons (parse-line str) (loop))))))))
