#lang racket

(require racket/list)

(provide count-fresh-ids)

(define (parse-range line)
  (match (regexp-match #px"^(\\d+)-(\\d+)$" line)
    [(list _ a b) (cons (string->number a)
                        (string->number b))]))

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
