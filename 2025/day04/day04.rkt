#lang racket

(require racket/list)

(provide count-accessible-rolls)

(define dirs
  '((-1 -1) (-1 0) (-1 1)
    (0  -1)         (0  1)
    (1  -1) (1  0)  (1  1)))

(define (grid-size g)
  (values (length g) (string-length (first g))))

(define (cell g r c)
  (string-ref (list-ref g r) c))

(define (adj-count g rows cols r c)
  (for/sum ([d dirs])
    (let ([rr (+ r (first d))]
          [cc (+ c (second d))])
      (if (and (<= 0 rr) (< rr rows)
               (<= 0 cc) (< cc cols)
               (char=? (cell g rr cc) #\@))
          1
          0))))

(define (accessible? g rows cols r c)
  (and (char=? (cell g r c) #\@)
       (< (adj-count g rows cols r c) 4)))

(define (count-accessible-rolls g)
  (define-values (rows cols) (grid-size g))
  (for*/sum ([r (in-range rows)]
             [c (in-range cols)]
             #:when (accessible? g rows cols r c))
    1))
