#lang racket

(provide square-digit?
         sum-square-digits-in-ranges)

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
