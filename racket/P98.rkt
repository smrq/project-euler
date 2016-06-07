#lang racket
(require "digits.rkt")

(define word-vector
  (call-with-input-file "P98_words.txt"
    (lambda (in)
      (list->vector
        (map (lambda (word)
               (substring word 1 (sub1 (string-length word))))
             (string-split (read-line in) ","))))))

(define (anagram? word1 word2)
  (and (= (string-length word1) (string-length word2))
       (equal? (sort (string->list word1) char<?)
               (sort (string->list word2) char<?))))

(define word-anagram-pairs
  (let ([pairs null])
    (for* ([i2 (in-range 1 (vector-length word-vector))]
           [i1 (in-range i2)])
      (let ([word1 (vector-ref word-vector i1)]
            [word2 (vector-ref word-vector i2)])
        (when (anagram? word1 word2)
          (set! pairs (cons (cons word1 word2) pairs)))))
    pairs))

(define (word-pair->pattern pair)
  (let* ([word1 (car pair)]
         [word2 (cdr pair)]
         [character-key (make-hash)]
         [word-length (string-length word1)]
         [n 0]
         [pattern1
          (for/list ([i (in-range word-length)])
            (hash-ref character-key
                      (string-ref word1 i)
                      (lambda ()
                        (hash-set! character-key
                                   (string-ref word1 i)
                                   n)
                        (set! n (add1 n))
                        (sub1 n))))]
         [pattern2
          (for/list ([i (in-range word-length)])
            (hash-ref character-key
                      (string-ref word2 i)
                      (lambda () (error (string-append word1 " and " word2 " are not anagrams")))))])
    (cons pattern1 pattern2)))

(define patterns
  (let ([v (build-vector (add1 9) (lambda (_) (set)))]
        [pattern-list (map word-pair->pattern word-anagram-pairs)])
    (for ([p pattern-list])
      (let ([p-length (length (car p))])
        (vector-set! v p-length (set-add (vector-ref v p-length) p))))
    v))

patterns

(define squares
  (let loop ([n 4] [acc (set)])
    (let ([nsq (* n n)])
      (if (< nsq 1000000000)
          (loop (add1 n) (set-add acc nsq))
          acc))))

(define (apply-pattern n pattern)
  (call/cc (lambda (break)
    (let ([pattern1 (car pattern)]
          [pattern2 (cdr pattern)]
          [digits1 (digits n)]
          [digit-key (make-hash)]
          [keyed-digits (mutable-set)])
      (for ([p pattern1]
            [d digits1])
        (when (and (hash-has-key? digit-key p)
                   (not (equal? (hash-ref digit-key p) d)))
          (break #f))
        (when (and (not (hash-has-key? digit-key p))
                   (set-member? keyed-digits d))
          (break #f))
        (hash-set! digit-key p d)
        (set-add! keyed-digits d))
      (digits->number
        (for/list ([p pattern2])
          (hash-ref digit-key p)))))))

(define (apply-all-patterns n patterns)
  (let ([n-length (number-length n)])
    (let ([ps (vector-ref patterns n-length)])
      (set-map ps (lambda (p) (apply-pattern n p))))))

(let ([max-square 0])
  (for* ([n squares]
         #:when (> n max-square)
         [m (apply-all-patterns n patterns)]
         #:when (and (set-member? squares m)
                     (= (number-length n) (number-length m)))) ; No leading zeros!
    (displayln (string-append (~a n) "," (~a m)))
    (set! max-square (max n m)))
  max-square)
