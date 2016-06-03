#lang racket

(define (string-replace-pairs str pairs)
  (foldl (lambda (pair acc)
           (string-replace acc (car pair) (cdr pair)))
         str pairs))

(define (additive-roman str)
  (string-replace-pairs
    str
    '(("IV" . "IIII")
      ("IX" . "IIIIIIIII")
      ("XL" . "XXXX")
      ("XC" . "XXXXXXXXX")
      ("CD" . "CCCC")
      ("CM" . "CCCCCCCCC"))))

(define (roman-value ch)
  (cond
    [(eq? ch #\I) 1]
    [(eq? ch #\V) 5]
    [(eq? ch #\X) 10]
    [(eq? ch #\L) 50]
    [(eq? ch #\C) 100]
    [(eq? ch #\D) 500]
    [(eq? ch #\M) 1000]
    [else 0]))

(define (parse-roman str)
  (let loop ([str (additive-roman str)]
             [acc 0])
    (if (zero? (string-length str))
        acc
        (loop (substring str 1)
              (+ acc (roman-value (string-ref str 0)))))))

(define (generate-roman n)
  (cond
    [(>= n 1000) (string-append "M" (generate-roman (- n 1000)))]
    [(>= n 900) (string-append "CM" (generate-roman (- n 900)))]
    [(>= n 500) (string-append "D" (generate-roman (- n 500)))]
    [(>= n 400) (string-append "CD" (generate-roman (- n 400)))]
    [(>= n 100) (string-append "C" (generate-roman (- n 100)))]
    [(>= n 90) (string-append "XC" (generate-roman (- n 90)))]
    [(>= n 50) (string-append "L" (generate-roman (- n 50)))]
    [(>= n 40) (string-append "XL" (generate-roman (- n 40)))]
    [(>= n 10) (string-append "X" (generate-roman (- n 10)))]
    [(>= n 9) (string-append "IX" (generate-roman (- n 9)))]
    [(>= n 5) (string-append "V" (generate-roman (- n 5)))]
    [(>= n 4) (string-append "IV" (generate-roman (- n 4)))]
    [(>= n 1) (string-append "I" (generate-roman (- n 1)))]
    [else ""]))

(define (get-roman-compression str)
  (- (string-length str)
     (string-length (generate-roman (parse-roman str)))))

(call-with-input-file "P89_roman.txt"
  (lambda (in)
    (let loop ([acc 0])
      (let ([line (read-line in)])
        (if (eq? line eof)
            acc
            (loop (+ acc (get-roman-compression line))))))))
