#lang racket

(require racket/list)

(provide count-accessible-rolls
         total-removable-rolls)

; Shared
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

; Part 1
(define (count-accessible-rolls g)
  (define-values (rows cols) (grid-size g))
  (for*/sum ([r (in-range rows)]
             [c (in-range cols)]
             #:when (accessible? g rows cols r c))
    1))

; Part 2
(define (transform-row g rows cols r)
  (for/fold ([chars '()] [removed 0]) ([c (in-range cols)])
    (let* ([ch (cell g r c)]
           [rem? (and (char=? ch #\@)
                      (< (adj-count g rows cols r c) 4))])
      (values (cons (if rem? #\. ch) chars)
              (if rem? (add1 removed) removed))
    )))

(define (remove-accessible g)
  (define-values (rows cols) (grid-size g))
  (define-values (rev-rows removed)
    (for/fold ([acc-rows '()] [acc-removed 0]) ([r (in-range rows)])
      (define-values (chars row-removed) (transform-row g rows cols r))
      (values (cons (list->string (reverse chars)) acc-rows)
              (+ acc-removed row-removed))))
  (values (reverse rev-rows) removed))

(define (total-removable-rolls g)
  (let loop ([grid g] [acc 0])
    (define-values (g2 removed) (remove-accessible grid))
    (if (zero? removed)
        acc
        (loop g2 (+ acc removed)))
    ))
