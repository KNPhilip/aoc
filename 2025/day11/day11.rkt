#lang racket

(provide find-total-paths)

(define (find-total-paths lines)
  (let* ([parse-line
          (lambda (line)
            (let* ([parts (string-split line ":")]
                   [src   (string-trim (first parts))]
                   [outs  (if (> (length parts) 1)
                              (map string-trim
                                   (string-split (second parts)))
                              '())])
              (cons src outs)))]
         [edges     (map parse-line lines)]
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
                       (cons node p)))]))])
      (length (dfs "you" (set "you")))
    )))
