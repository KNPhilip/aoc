#lang racket

(provide square-digit?
         sum-square-digits-in-ranges
         repeating-pattern?
         sum-periodic-digits-in-ranges)

; Part 1
(define (square-digit? n)
  (let* ([s (number->string n)]
         [len (string-length s)])
    (and
      (even? len)
      (let* ([half (/ len 2)]
             [a (substring s 0 half)]
             [b (substring s half len)])
        (string=? a b)))
  ))

(define (sum-square-digits start end)
  (let ([sum 0]
        [start (string->number start)]
        [end (string->number end)]) 
    (for ([n (in-range start (add1 end))])
      (when (square-digit? n)
        (set! sum (+ sum n))))
    sum))

(define (sum-square-digits-in-ranges input)
  (let ([sum 0])
    (for ([range (string-split input ",")])
      (match (string-split range "-")
        [(list s1 s2)
          (set! sum (+ sum (sum-square-digits s1 s2)))
        ]))
  sum))

; Part 2
(define (repeating-pattern? n)
  (let* ([s (number->string n)]
         [len (string-length s)])
    (for/or ([i (in-range 1 len)]
             #:when (zero? (modulo len i)))
      (let ([list (make-list (/ len i) (substring s 0 i))])
        (string=? s (apply string-append list)))
      )))

; Below is just the same code as part one.
(define (sum-periodic-digits start end)
  (let ([sum 0]
        [start (string->number start)]
        [end (string->number end)]) 
    (for ([n (in-range start (add1 end))])
      (when (repeating-pattern? n)
        (set! sum (+ sum n))))
    sum))

(define (sum-periodic-digits-in-ranges input)
  (let ([sum 0])
    (for ([range (string-split input ",")])
      (match (string-split range "-")
        [(list s1 s2)
          (set! sum (+ sum (sum-periodic-digits s1 s2)))
        ]))
  sum))
