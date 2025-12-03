#lang racket

(provide sum-highest-joltage-pairs-of)

(define (highest-joltages-of input)
  (let ([digits (map (lambda (c) (- (char->integer c) (char->integer #\0))) (string->list input))])
    (define (loop max-joltage i j)
      (if (>= i (length digits))
          max-joltage
          (if (>= j (length digits))
              (loop max-joltage (+ i 1) (+ i 2))
              (let* ([first (list-ref digits i)]
                     [second (list-ref digits j)]
                     [current-joltage (+ (* 10 first) second)])
                (loop (max max-joltage current-joltage) i (+ j 1))))))
    (loop 0 0 1)))

(define (sum-highest-joltage-pairs-of input-list)
  (apply + (map highest-joltages-of input-list)))
