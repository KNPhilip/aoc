#lang racket

(require racket/list)

(provide count-fresh-ids
         count-fresh-range-ids)

;; Shared
(define (parse-range line)
  (match (regexp-match #px"^(\\d+)-(\\d+)$" line)
    [(list _ a b) (cons (string->number a)
                        (string->number b))]))

;; Part 1
(define (fresh? n ranges)
  (for/or ([r ranges])
    (<= (car r) n (cdr r))))

(define (count-fresh-ids input)
  (let* ([parts       (string-split input "\n\n")]
         [range-lines (string-split (first parts) "\n" #:trim? #t)]
         [id-lines    (string-split (second parts) "\n" #:trim? #t)]
         [ranges      (map parse-range range-lines)]
         [ids         (map string->number id-lines)])
  (for/sum ([id ids])
    (if (fresh? id ranges) 1 0))))

;; Part 2
(define (merge-ranges ranges)
  (let sorted ([sorted (sort ranges < #:key car)])
    (reverse
     (for/fold ([acc '()]) ([r sorted])
       (match acc
         ['() (list r)]
         [(cons (cons ls le) rest)
          (if (<= (car r) (add1 le))
              (cons (cons ls (max le (cdr r))) rest)
              (cons r acc))]))
     )))

(define (count-fresh-range-ids input)
  (let* ([parts (string-split input "\n\n" #:trim? #t)]
         [range-lines (if (null? parts) '()
                          (string-split (first parts) "\n" #:trim? #t))])
    (for/sum ([r (merge-ranges (map parse-range range-lines))])
      (add1 (- (cdr r) (car r))))
  ))
