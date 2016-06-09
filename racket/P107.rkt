#lang racket
(require "file.rkt")

(define network
  (list->vector
    (parse-file "P107_network.txt"
                (lambda (str)
                  (for/vector ([t (string-split str ",")])
                    (if (equal? t "-")
                        +inf.0
                        (string->number t)))))))

(define original-network-weight
  (/ (for/sum ([row network])
       (for/sum ([n row])
         (if (= n +inf.0) 0 n)))
     2))

(define (find-cheapest-link)
  (let ([best #f]
        [cost +inf.0])
    (for* ([i (in-range (vector-length network))]
           [j (in-range (vector-length network))])
      (let ([ij-cost (vector-ref (vector-ref network i) j)])
        (when (< ij-cost cost)
          (set! best (cons i j))
          (set! cost ij-cost))))
    (values (car best) (cdr best))))

(define (members-of-set set-membership n)
  (for/list ([i (in-range (vector-length set-membership))]
             #:when (= (vector-ref set-membership i) n))
    i))

(define (prune-links members)
  (for* ([a members]
         [b members])
    (vector-set! (vector-ref network a) b +inf.0)
    (vector-set! (vector-ref network b) a +inf.0)))

(define new-network-weight
  (let ([set-membership (make-vector (vector-length network) 0)]
        [total-weight 0]
        [next-set 1])
    (for ([i (in-range (sub1 (vector-length network)))])
      (let-values ([(link-a link-b) (find-cheapest-link)])
        (displayln (string-append "Connecting " (~a link-a) " with " (~a link-b)))
        (set! total-weight (+ total-weight (vector-ref (vector-ref network link-a) link-b)))
        (cond
          [(and (zero? (vector-ref set-membership link-a))
                (zero? (vector-ref set-membership link-b)))
           (vector-set! set-membership link-a next-set)
           (vector-set! set-membership link-b next-set)
           (set! next-set (add1 next-set))]
          [(zero? (vector-ref set-membership link-a))
           (vector-set! set-membership link-a (vector-ref set-membership link-b))]
          [(zero? (vector-ref set-membership link-b))
           (vector-set! set-membership link-b (vector-ref set-membership link-a))]
          [else
           (let ([b-set (members-of-set set-membership (vector-ref set-membership link-b))])
             (for ([member b-set])
               (vector-set! set-membership member (vector-ref set-membership link-a))))])
        (prune-links (members-of-set set-membership (vector-ref set-membership link-a)))))
    total-weight))

(- original-network-weight new-network-weight)
