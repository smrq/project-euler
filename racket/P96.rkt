#lang racket
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
  (let ([i0 (* 3 (quotient b 3))]
        [j0 (* 3 (modulo b 3))])
    (for*/vector ([i (in-range i0 (+ 3 i0))]
                  [j (in-range j0 (+ 3 j0))])
      (cell-ref puzzle i j))))

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

  (define (remove-digit! i j digit)
    (let ([cell (cell-ref puzzle i j)])
      (and (set? cell)
           (set-member? cell digit)
           (begin (cell-set! puzzle i j
                             (if (= (set-count cell) 2)
                                 (set-first (set-remove cell digit))
                                 (set-remove cell digit)))
                  #t))))

  (define (remove-conflicts! i j conflicts)
    (let* ([c (cell-ref puzzle i j)]
           [c-new (set-subtract c conflicts)])
      (and (not (equal? c c-new))
           (begin (cell-set! puzzle i j
                             (if (= (set-count c-new) 1)
                                 (set-first c-new)
                                 c-new))
                  #t))))

  (define (box-cell-contains-possible-digit b n digit)
    (let ([cell (cell-ref-box puzzle b n)])
      (and (set? cell)
           (set-member? cell digit))))

  (define (box-row-contains-possible-digit b nr d)
    (or (box-cell-contains-possible-digit b (* nr 3) d)
        (box-cell-contains-possible-digit b (+ (* nr 3) 1) d)
        (box-cell-contains-possible-digit b (+ (* nr 3) 2) d)))

  (define (box-col-contains-possible-digit b nc d)
    (or (box-cell-contains-possible-digit b nc d)
        (box-cell-contains-possible-digit b (+ nc 3) d)
        (box-cell-contains-possible-digit b (+ nc 6) d)))

  (define (in-box? b i j)
    (and (= (quotient b 3) (quotient i 3))
         (= (modulo b 3) (quotient j 3))))

  (define (narrow-expand-single-box-lines! b)
    (let* ([altered? #f]
           [digits-solved (for/set ([d (puzzle-box puzzle b)] #:when (number? d)) d)]
           [digits-unsolved (set-subtract (set 1 2 3 4 5 6 7 8 9) digits-solved)])
      (let-values ([(bi bj) (cell-box-ij b 0)])
        (for ([digit digits-unsolved])
          (let ([i (cond
                     [(and (box-row-contains-possible-digit b 0 digit)
                           (not (box-row-contains-possible-digit b 1 digit))
                           (not (box-row-contains-possible-digit b 2 digit)))
                      bi]
                     [(and (not (box-row-contains-possible-digit b 0 digit))
                           (box-row-contains-possible-digit b 1 digit)
                           (not (box-row-contains-possible-digit b 2 digit)))
                      (+ bi 1)]
                     [(and (not (box-row-contains-possible-digit b 0 digit))
                           (not (box-row-contains-possible-digit b 1 digit))
                           (box-row-contains-possible-digit b 2 digit))
                      (+ bi 2)]
                     [else #f])])
            (when i
              (for ([j (in-range 9)]
                    #:when (not (in-box? b i j)))
                (when (and (not (puzzle-cell-solved? i j))
                           (remove-digit! i j digit))
                  (set! altered? #t)))))
          (let ([j (cond
                     [(and (box-col-contains-possible-digit b 0 digit)
                           (not (box-col-contains-possible-digit b 1 digit))
                           (not (box-col-contains-possible-digit b 2 digit)))
                      bj]
                     [(and (not (box-col-contains-possible-digit b 0 digit))
                           (box-col-contains-possible-digit b 1 digit)
                           (not (box-col-contains-possible-digit b 2 digit)))
                      (+ bj 1)]
                     [(and (not (box-col-contains-possible-digit b 0 digit))
                           (not (box-col-contains-possible-digit b 1 digit))
                           (box-col-contains-possible-digit b 2 digit))
                      (+ bj 2)]
                     [else #f])])
            (when j
              (for ([i (in-range 9)]
                    #:when (not (in-box? b i j)))
                (when (and (not (puzzle-cell-solved? i j))
                           (remove-digit! i j digit))
                  (set! altered? #t)))))
      ))
      altered?))

  (define (narrow-expand-single-box-lines-all!)
    (let ([altered? #f])
      (for ([b (in-range 9)])
        (when (narrow-expand-single-box-lines! b)
          (set! altered? #t)))
      altered?))

  (define (cols-in-row-with-possible-digit i digit)
    (for/set ([j (in-range 9)]
              #:when (let ([cell (cell-ref puzzle i j)])
                       (and (set? cell)
                            (set-member? cell digit))))
      j))

  (define (rows-in-col-with-possible-digit j digit)
    (for/set ([i (in-range 9)]
              #:when (let ([cell (cell-ref puzzle i j)])
                       (and (set? cell)
                            (set-member? cell digit))))
      i))

  (define (narrow-xwing-row! digit i1 i2)
    (let* ([altered? #f])
      (let ([js1 (cols-in-row-with-possible-digit i1 digit)]
            [js2 (cols-in-row-with-possible-digit i2 digit)])
        (when (and (= 2 (set-count js1))
                   (equal? js1 js2))
          (for* ([j js1]
                 [i (in-range 9)]
                 #:when (not (or (= i i1) (= i i2))))
            (when (and (not (puzzle-cell-solved? i j))
                       (remove-digit! i j digit))
              (set! altered? #t)))))
      altered?))

  (define (narrow-xwing-col! digit j1 j2)
    (let* ([altered? #f])
      (let ([is1 (rows-in-col-with-possible-digit j1 digit)]
            [is2 (rows-in-col-with-possible-digit j2 digit)])
        (when (and (= 2 (set-count is1))
                   (equal? is1 is2))
          (for* ([i is1]
                 [j (in-range 9)]
                 #:when (not (or (= j j1) (= j j2))))
            (when (and (not (puzzle-cell-solved? i j))
                       (remove-digit! i j digit))
              (set! altered? #t)))))
      altered?))

  (define (narrow-xwing!)
    (let* ([altered? #f])
      (for* ([digit (in-range 9)]
             [i2 (in-range 1 9)]
             [i1 (in-range i2)])
        (when (narrow-xwing-row! digit i1 i2)
          (set! altered? #t)))
      (for* ([digit (in-range 9)]
             [j2 (in-range 1 9)]
             [j1 (in-range j2)])
        (when (narrow-xwing-col! digit j1 j2)
          (set! altered? #t)))
      altered?))

  (gen-trivial-candidates!)
  (do-while-true (lambda () (and (not (puzzle-solved?))
                                 (or (narrow-only-possible-digit!)
                                     (narrow-remove-direct-conflicts!)
                                     (narrow-remove-mutex!)
                                     (narrow-expand-single-box-lines-all!)
                                     (narrow-xwing!)))))

  puzzle
)

;----------------------------------------------------------------------

(apply + (map (lambda (puzzle)
                (+ (* 100 (cell-ref puzzle 0 0))
                   (* 10 (cell-ref puzzle 0 1))
                   (cell-ref puzzle 0 2)))
              (for/list ([puzzle puzzles])
                (solve-puzzle! puzzle))))
