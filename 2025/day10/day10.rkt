#lang racket

(provide fewest-button-presses)

(require racket/string
         racket/list)

;; Part 1 is done. Gave up on part 2.

(define (parse-diagram s)
  (map (lambda (c) (if (char=? c #\#) 1 0))
       (string->list (substring s 1 (sub1 (string-length s))))))

(define (parse-button s)
  (let* ([inside  (substring s 1 (sub1 (string-length s)))]
         [trimmed (string-trim inside)])
    (if (non-empty-string? trimmed)
        (map string->number (string-split trimmed "," #:trim? #t))
        '())))

(define (parse-line line)
  (let* ([chunks (regexp-match* #px"\\[[^]]*\\]|\\([^)]*\\)|\\{[^}]*\\}" line)]
         [diagram-str (car chunks)]
         [button-strs (filter (λ (p) (regexp-match #px"^\\(" p)) chunks)])
    (list (parse-diagram diagram-str)
          (map parse-button button-strs))))

(define (button->vec indices total-lights)
  (for/list ([i (in-range total-lights)])
    (if (member i indices) 1 0)))

(define (xor-of-buttons button-vecs mask)
  (for/fold ([acc (make-list (length (car button-vecs)) 0)])
            ([i (in-range (length button-vecs))])
    (if (bitwise-bit-set? mask i)
        (map bitwise-xor acc (list-ref button-vecs i))
        acc)))

(define (count-bits x)
  (if (zero? x)
      0
      (+ (bitwise-and x 1) (count-bits (arithmetic-shift x -1)))))

(define (minimum-presses target button-lists)
  (let* ([num-lights (length target)]
         [button-vecs (map (λ (b) (button->vec b num-lights)) button-lists)]
         [num-buttons (length button-vecs)]
         [limit       (expt 2 num-buttons)]
         [subset-info
          (for/list ([mask (in-range limit)])
            (cons mask (count-bits mask)))]
         [valid-subsets
          (filter (lambda (pr)
                    (equal? (xor-of-buttons button-vecs (car pr))
                            target))
                  subset-info)]
         [press-counts (map cdr valid-subsets)])
    (if (null? press-counts)
        0
        (apply min press-counts))))

(define (fewest-button-presses lines)
  (let* ([parsed (map parse-line lines)]
         [mins   (map (lambda (p)
                        (minimum-presses (first p) (second p)))
                      parsed)])
    (apply + mins)))
