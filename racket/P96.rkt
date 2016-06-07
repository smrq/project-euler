#lang racket
(require racket/trace)
(require "memo.rkt")

(define-memoized (n-permutations k n)
  (let loop ([k k] [max n])
    (cond
      [(= k 1) (for/list ([i (in-range (sub1 k) max)]) (list i))]
      [else (for*/list ([i (in-range (sub1 k) max)]
                        [ls (loop (sub1 k) i)])
              (cons i ls))])))

(define (cell-n i j)
  (+ (* 9 i) j))

(define (cell-box-ij b n)
  (let ([i (+ (* 3 (quotient b 3))
              (quotient n 3))]
        [j (+ (* 3 (modulo b 3))
              (modulo n 3))])
    (values i j)))

(define (cell-set! puzzle i j value)
  (vector-set! puzzle (cell-n i j) value))

(define (cell-box-set! puzzle b n value)
  (let-values ([(i j) (cell-box-ij b n)])
    (cell-set! puzzle i j value)))

(define (cell-set-candidates! puzzle i j c)
  (when (equal? c (set))
    (display-puzzle puzzle)
    (error (string-append "Candidates empty at " (~a i) "," (~a j))))
  (cell-set! puzzle i j
             (if (= (set-count c) 1)
                 (set-first c)
                 c)))

(define (cell-set-candidates-box! puzzle b n c)
  (when (equal? c (set))
    (display-puzzle puzzle)
    (error (string-append "Candidates empty at box." (~a b) "," (~a n))))
  (let-values ([(i j) (cell-box-ij b n)])
    (cell-set-candidates! puzzle i j c)))

(define (cell-ref puzzle i j)
  (vector-ref puzzle (cell-n i j)))

(define (cell-ref-box puzzle b n)
  (let-values ([(i j) (cell-box-ij b n)])
    (cell-ref puzzle i j)))

(define (puzzle-row puzzle i)
  (for/vector ([j (in-range 9)])
    (cell-ref puzzle i j)))

(define (puzzle-col puzzle j)
  (for/vector ([i (in-range 9)])
    (cell-ref puzzle i j)))

(define (puzzle-box puzzle b)
  (for*/vector ([i (in-range (quotient b 3) (+ 3 (quotient b 3)))]
                [j (in-range (modulo b 3) (+ 3 (modulo b 3)))])
    (cell-ref puzzle i j)))

(define (puzzle-row-without-coord puzzle i j)
  (for*/set ([c (in-range 9)]
                #:when (not (= j c)))
    (cell-ref puzzle i c)))

(define (puzzle-col-without-coord puzzle i j)
  (for*/set ([r (in-range 9)]
                #:when (not (= i r)))
    (cell-ref puzzle r j)))

(define (puzzle-box-without-coord puzzle i j)
  (let ([r (* 3 (quotient i 3))]
        [c (* 3 (quotient j 3))])
    (for*/set ([r (in-range r (+ r 3))]
                  [c (in-range c (+ c 3))]
                  #:when (not (and (= i r) (= j c))))
      (cell-ref puzzle r c))))

;----------------------------------------------------------------------

(define (parse-puzzle in)
  (let ([puzzle (make-vector 81)])
    (read-line in) ; Grid NN
    (for ([i (in-range 9)])
      (for ([j (in-range 9)])
        (cell-set! puzzle i j (string->number (read-string 1 in))))
      (read-line in))
    puzzle))

(define puzzles
  (call-with-input-file "P96_sudoku.txt"
    (lambda (in)
      (let loop-puzzle ([puzzles null])
        (if (eof-object? (peek-string 1 0 in))
            puzzles
            (loop-puzzle (cons (parse-puzzle in) puzzles)))))))

(define (display-puzzle puzzle)
  (displayln "--------------------")
  (for ([i (in-range 9)])
    (displayln (puzzle-row puzzle i)))
  (displayln "--------------------"))

;----------------------------------------------------------------------

(define (do-while-true fn)
  (let loop () (when (fn) (loop))))

(define (solve-puzzle! puzzle)

  (define (puzzle-cell-solved? i j)
    (number? (cell-ref puzzle i j)))

  (define (puzzle-solved?)
    (for*/and ([i (in-range 9)]
              [j (in-range 9)])
      (puzzle-cell-solved? i j)))

  (define (set-only-literals s)
    (for/set ([n s]
              #:when (number? n))
      n))

  (define (set-has-potential-digit? s d)
    (or (set-member? s d)
        (for/or ([ds s] #:when (set? ds))
          (set-member? ds d))))

  (define (gen-trivial-candidates!)
    (for* ([i (in-range 9)]
           [j (in-range 9)]
           #:when (zero? (cell-ref puzzle i j)))
      (cell-set! puzzle i j (set 1 2 3 4 5 6 7 8 9))))

  (define (narrow-remove-direct-conflicts-cell! i j)
    (let ([candidates (cell-ref puzzle i j)]
          [row (set-only-literals (puzzle-row-without-coord puzzle i j))]
          [col (set-only-literals (puzzle-col-without-coord puzzle i j))]
          [box (set-only-literals (puzzle-box-without-coord puzzle i j))])
      (let ([conflicts (set-union row col box)])
        (let ([new-candidates (set-subtract candidates conflicts)])
          (and (not (equal? candidates new-candidates))
               (begin
                 ; (displayln (string-append "narrow-remove-direct-conflicts " (~a i) "," (~a j) " " (~a new-candidates)))
                 (cell-set-candidates! puzzle i j new-candidates)
                 #t))))))

  (define (narrow-remove-direct-conflicts!)
    (let ([altered? #f])
      (for* ([i (in-range 9)]
             [j (in-range 9)]
             #:when (not (puzzle-cell-solved? i j)))
        (when (narrow-remove-direct-conflicts-cell! i j)
          (set! altered? #t)))
      altered?))

  (define (narrow-only-possible-digit-cell! i j)
    (let ([row (puzzle-row-without-coord puzzle i j)]
          [col (puzzle-col-without-coord puzzle i j)]
          [box (puzzle-box-without-coord puzzle i j)])
      (call/cc (lambda (break)
        (for ([d (in-range 1 10)])
          (when (and (not (set-has-potential-digit? row d))
                     (not (set-has-potential-digit? col d))
                     (not (set-has-potential-digit? box d)))
            ; (displayln (string-append "narrow-only-possible-digit " (~a i) (~a j) (~a d)))
            (cell-set! puzzle i j d)
            (break #t)))
        #f))))

  (define (narrow-only-possible-digit!)
    (let ([altered? #f])
      (for* ([i (in-range 9)]
             [j (in-range 9)]
             #:when (not (puzzle-cell-solved? i j)))
        (when (narrow-only-possible-digit-cell! i j)
          (set! altered? #t)))
      altered?))

  (define (narrow-remove-mutex-row! i)
    (call/cc (lambda (break)
      (for* ([k (in-range 2 8)]
             [js (n-permutations k 9)])
        (let ([cs (map (lambda (j) (cell-ref puzzle i j)) js)])
          (when (andmap (lambda (c) (set? c)) cs)
            (let ([cunion (apply set-union cs)])
              (when (<= (set-count cunion) k)
                (for ([j (in-range 9)] #:when (andmap (lambda (jj) (not (= j jj))) js))
                  (let ([c (cell-ref puzzle i j)])
                    (when (set? c)
                      (let ([c-new (set-subtract c cunion)])
                        (when (not (equal? c c-new))
                          (cell-set-candidates! puzzle i j c-new)
                          (break #t)))))))))))
      #f)))

  (define (narrow-remove-mutex-col! j)
    (call/cc (lambda (break)
      (for* ([k (in-range 2 8)]
             [is (n-permutations k 9)])
        (let ([cs (map (lambda (i) (cell-ref puzzle i j)) is)])
          (when (andmap (lambda (c) (set? c)) cs)
            (let ([cunion (apply set-union cs)])
              (when (<= (set-count cunion) k)
                (for ([i (in-range 9)] #:when (andmap (lambda (ii) (not (= i ii))) is))
                  (let ([c (cell-ref puzzle i j)])
                    (when (set? c)
                      (let ([c-new (set-subtract c cunion)])
                        (when (not (equal? c c-new))
                          (cell-set-candidates! puzzle i j c-new)
                          (break #t)))))))))))
      #f)))

  (define (narrow-remove-mutex-box! b)
    (call/cc (lambda (break)
      (for* ([k (in-range 2 8)]
             [ns (n-permutations k 9)])
        (let ([cs (map (lambda (n) (cell-ref-box puzzle b n)) ns)])
          (when (andmap (lambda (c) (set? c)) cs)
            (let ([cunion (apply set-union cs)])
              (when (<= (set-count cunion) k)
                (for ([n (in-range 9)] #:when (andmap (lambda (nn) (not (= n nn))) ns))
                  (let ([c (cell-ref-box puzzle b n)])
                    (when (set? c)
                      (let ([c-new (set-subtract c cunion)])
                        (when (not (equal? c c-new))
                          (cell-set-candidates-box! puzzle b n c-new)
                          (break #t)))))))))))
      #f)))

  (define (narrow-remove-mutex!)
    (let ([altered? #f])
      (for ([i (in-range 9)])
        (set! altered? (or altered? (narrow-remove-mutex-row! i))))
      (for ([j (in-range 9)])
        (set! altered? (or altered? (narrow-remove-mutex-col! j))))
      (for ([b (in-range 9)])
        (set! altered? (or altered? (narrow-remove-mutex-box! b))))
      altered?))

  (define (remove-conflicts! i j conflicts)
    (let* ([c (cell-ref puzzle i j)]
           [c-new (set-subtract c conflicts)])
      (and (not (equal? c c-new))
           (begin (cell-set! puzzle i j
                             (if (= (set-count c-new) 1)
                                 (set-first c-new)
                                 c-new))
                  #t))))

  (define (digits-exclusively-in-box-positions b ns)
    (let ([otherNs (for/list ([i (in-range 9)]
                              #:when (not (member i ns)))
                     i)])
      (let ([cs (filter set? (map (lambda (n) (cell-ref-box puzzle b n)) ns))]
            [otherCs (filter set? (map (lambda (n) (cell-ref-box puzzle b n)) otherNs))])
        (set-subtract (apply set-union cs)
                      (if (null? otherCs)
                          (set)
                          (apply set-union otherCs))))))

  ; wow what a terrible way to write this code!!
  (define (narrow-box-single-row! b)
    (call/cc (lambda (break)
      (for* ([n0 (in-range 0 9 3)])
        (let* ([n1 (+ n0 1)]
               [n2 (+ n0 2)]
               [c0 (cell-ref-box puzzle b n0)]
               [c1 (cell-ref-box puzzle b n1)]
               [c2 (cell-ref-box puzzle b n2)])
          (let-values ([(bi0 bj0) (cell-box-ij b n0)]
                       [(bi1 bj1) (cell-box-ij b n1)]
                       [(bi2 bj2) (cell-box-ij b n2)])
            (when (and (set? c0) (set? c1) (set? c2))
              (let ([digits (digits-exclusively-in-box-positions b (list n0 n1 n2))])
                (when (<= (set-count digits) 3)
                  (let ([altered? #f])
                    (for ([j (in-range 9)]
                          #:when (not (or (= j bj0) (= j bj1) (= j bj2))))
                      (let ([c (cell-ref puzzle bi0 j)])
                        (when (and
                                (not (puzzle-cell-solved? bi0 j))
                                (remove-conflicts! bi0 j digits))
                          (set! altered? #t))))
                    (when altered? (break #t))))))

            (when (and (set? c0) (set? c1))
              (let ([digits (digits-exclusively-in-box-positions b (list n0 n1))])
                (when (<= (set-count digits) 3)
                  (let ([altered? #f])
                    (for ([j (in-range 9)]
                          #:when (not (or (= j bj0) (= j bj1))))
                      (let ([c (cell-ref puzzle bi0 j)])
                        (when (and
                                (not (puzzle-cell-solved? bi0 j))
                                (remove-conflicts! bi0 j digits))
                          (set! altered? #t))))
                    (when altered? (break #t))))))

            (when (and (set? c0) (set? c2))
              (let ([digits (digits-exclusively-in-box-positions b (list n0 n2))])
                (when (<= (set-count digits) 3)
                  (let ([altered? #f])
                    (for ([j (in-range 9)]
                          #:when (not (or (= j bj0) (= j bj2))))
                      (let ([c (cell-ref puzzle bi0 j)])
                        (when (and
                                (not (puzzle-cell-solved? bi0 j))
                                (remove-conflicts! bi0 j digits))
                          (set! altered? #t))))
                    (when altered? (break #t))))))

            (when (and (set? c2) (set? c1))
              (let ([digits (digits-exclusively-in-box-positions b (list n2 n1))])
                (when (<= (set-count digits) 3)
                  (let ([altered? #f])
                    (for ([j (in-range 9)]
                          #:when (not (or (= j bj2) (= j bj1))))
                      (let ([c (cell-ref puzzle bi0 j)])
                        (when (and
                                (not (puzzle-cell-solved? bi0 j))
                                (remove-conflicts! bi0 j digits))
                          (set! altered? #t))))
                    (when altered? (break #t)))))))))
      #f)))

  (define (narrow-box-single-col! b)
    (call/cc (lambda (break)
      (for* ([n0 (in-range 0 3)])
        (let* ([n1 (+ n0 3)]
               [n2 (+ n0 3)]
               [c0 (cell-ref-box puzzle b n0)]
               [c1 (cell-ref-box puzzle b n1)]
               [c2 (cell-ref-box puzzle b n2)])
          (let-values ([(bi0 bj0) (cell-box-ij b n0)]
                       [(bi1 bj1) (cell-box-ij b n1)]
                       [(bi2 bj2) (cell-box-ij b n2)])
            (when (and (set? c0) (set? c1) (set? c2))
              (let ([digits (digits-exclusively-in-box-positions b (list n0 n1 n2))])
                (when (<= (set-count digits) 3)
                  (let ([altered? #f])
                    (for ([i (in-range 9)]
                          #:when (not (or (= i bi0) (= i bi1) (= i bi2))))
                      (let ([c (cell-ref puzzle i bj0)])
                        (when (and
                                (not (puzzle-cell-solved? i bj0))
                                (remove-conflicts! i bj0 digits))
                          (set! altered? #t))))
                    (when altered? (break #t))))))

            (when (and (set? c0) (set? c1))
              (let ([digits (digits-exclusively-in-box-positions b (list n0 n1))])
                (when (<= (set-count digits) 3)
                  (let ([altered? #f])
                    (for ([i (in-range 9)]
                          #:when (not (or (= i bi0) (= i bi1))))
                      (let ([c (cell-ref puzzle i bj0)])
                        (when (and
                                (not (puzzle-cell-solved? i bj0))
                                (remove-conflicts! i bj0 digits))
                          (set! altered? #t))))
                    (when altered? (break #t))))))

            (when (and (set? c0) (set? c2))
              (let ([digits (digits-exclusively-in-box-positions b (list n0 n2))])
                (when (<= (set-count digits) 3)
                  (let ([altered? #f])
                    (for ([i (in-range 9)]
                          #:when (not (or (= i bi0) (= i bi2))))
                      (let ([c (cell-ref puzzle i bj0)])
                        (when (and
                                (not (puzzle-cell-solved? i bj0))
                                (remove-conflicts! i bj0 digits))
                          (set! altered? #t))))
                    (when altered? (break #t))))))

            (when (and (set? c2) (set? c1))
              (let ([digits (digits-exclusively-in-box-positions b (list n2 n1))])
                (when (<= (set-count digits) 3)
                  (let ([altered? #f])
                    (for ([i (in-range 9)]
                          #:when (not (or (= i bi2) (= i bi1))))
                      (let ([c (cell-ref puzzle i bj0)])
                        (when (and
                                (not (puzzle-cell-solved? i bj0))
                                (remove-conflicts! i bj0 digits))
                          (set! altered? #t))))
                    (when altered? (break #t)))))))))
      #f)))

  (define (narrow-box-single!)
    (let ([altered? #f])
      (for ([b (in-range 9)])
        (set! altered? (or altered? (narrow-box-single-row! b)))
        (set! altered? (or altered? (narrow-box-single-col! b)))
      )
      altered?))

  (gen-trivial-candidates!)
  (do-while-true (lambda () (and (not (puzzle-solved?))
                                 (or (narrow-only-possible-digit!)
                                     (narrow-remove-direct-conflicts!)
                                     (narrow-remove-mutex!)
                                     (narrow-box-single!)
                                     ))))

  puzzle
)

;----------------------------------------------------------------------

;(map (lambda (puzzle) (list (cell-ref puzzle 0 0) (cell-ref puzzle 0 1) (cell-ref puzzle 0 2)))

; (for/list ([puzzle puzzles])
(for ([puzzle (list (list-ref puzzles 2) (list-ref puzzles 43))])
  (display-puzzle (solve-puzzle! puzzle)))
