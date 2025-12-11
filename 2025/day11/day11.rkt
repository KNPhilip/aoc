#lang racket

(provide find-total-paths
         find-total-paths-2)

;; Shared
(define (parse-line line)
  (let* ([parts (string-split line ":")]
         [src   (string-trim (first parts))]
         [outs  (if (> (length parts) 1)
                    (map string-trim
                         (string-split (second parts)))
                         '())])
    (cons src outs)))

;; Part 1
(define (find-total-paths lines)
  (let* ([edges     (map parse-line lines)]
         [adjacency (for/hash ([p edges])
                      (values (car p) (cdr p)))])
    (letrec ([dfs
              (lambda (node visited)
                (cond
                  [(equal? node "out")
                   (list (list "out"))]
                  [else
                   (let* ([nexts (hash-ref adjacency node '())])
                     (for*/list ([n nexts]
                                 #:unless (set-member? visited n)
                                 [p (dfs n (set-add visited n))])
                       (cons node p)))]
                ))])
      (length (dfs "you" (set "you")))
    )))

;; Part 2
(define (find-total-paths-2 lines)
  (let ([adjacency
         (for/hash ([p (map parse-line lines)])
           (values (car p) (cdr p)))]
        [memo (make-hash)]
        [DAC? 1]
        [FFT? 2])
    (define (dfs node mask)
      (define newmask
        (cond [(equal? node "dac") (bitwise-ior mask DAC?)]
              [(equal? node "fft") (bitwise-ior mask FFT?)]
              [else mask]))
      (cond
        [(equal? node "out")
         (if (= newmask (bitwise-ior DAC? FFT?)) 1 0)]
        [else
         (define key (cons node newmask))
         (hash-ref! memo key
                    (lambda ()
                      (for/sum ([n (hash-ref adjacency node '())])
                        (dfs n newmask))))
        ]))
  (dfs "svr" 0)))
