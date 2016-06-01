#lang racket

(provide define-memoized)

(define (memoize fn)
  (let ([results (make-hash)])
    (lambda args
      (when (not (hash-has-key? results args))
        (hash-set! results args (apply fn args)))
      (hash-ref results args))))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ (name . args) . body)
     (define name (memoize (lambda args . body))))))
