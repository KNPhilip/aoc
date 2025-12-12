#lang racket

(require racket/list)

(provide solve)

(define (extract-parts lines)
  (let loop ([ls lines] [acc '()] [chunk '()])
    (cond
      [(null? ls) (reverse (cons (reverse chunk) acc))]
      [(string=? "" (car ls)) (loop (cdr ls) (cons (reverse chunk) acc) '())]
      [else (loop (cdr ls) acc (cons (car ls) chunk))]
    )))

(define (get-sizes presents)
  (let ([sizes (make-hash)])
    (for-each
      (lambda (present)
        (let* ([lines present]
               [name (string->number (substring (car lines) 0 (- (string-length (car lines)) 1)))]
               [G (map string->list (cdr lines))]
               [size (for*/sum ([row G] [c row]) (if (char=? c #\#) 1 0))])
          (hash-set! sizes name size)))
      presents)
    sizes))

(define (solve lines)
  (let* ([parts (extract-parts lines)]
         [presents (take parts (- (length parts) 1))]
         [regions (last parts)]
         [sizes (get-sizes presents)])
    (for/fold ([acc 0]) ([region regions])
      (let* ([parts (string-split region ": ")]
             [sz (first parts)]
             [ns-str (second parts)]
             [RC (map string->number (string-split sz "x"))]
             [R (first RC)]
             [C (second RC)]
             [ns (map string->number (string-split ns-str " "))]
             [total-present-size (for/sum ([i ns] [idx (in-naturals)]) (* i (hash-ref sizes idx)))]
             [total-grid-size (* R C)])
        (if (< (* total-present-size 1.3) total-grid-size)
            (+ acc 1)
            acc)))))
