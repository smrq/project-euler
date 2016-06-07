#lang racket

(define (log_k k n) (/ (log n) (log k)))

; a^x >? b^y
; c = log_b(a)
; cx > y <==> b^cx = a^x > b^y
(define (compare-big-expts a x b y)
  (let ([c (log_k b a)])
    (> (* c x) y)))

(require racket/trace)

(define (parse-line in)
  (let ([str (read-line in)])
    (if (eof-object? str)
        #f
        (map string->number
             (string-split str ",")))))

(call-with-input-file "P99_base_exp.txt"
  (lambda (in)
    (let loop ([biggest-pair (parse-line in)] [biggest-line-num 1] [line-num 2])
      (let ([pair (parse-line in)])
        (cond
          [(not pair) (values biggest-pair biggest-line-num)]
          [(compare-big-expts (first pair) (second pair) (first biggest-pair) (second biggest-pair))
           (loop pair line-num (add1 line-num))]
          [else
           (loop biggest-pair biggest-line-num (add1 line-num))])))))
