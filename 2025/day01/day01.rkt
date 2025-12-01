#lang racket

(provide count-safe-moves)

(define (update-position position direction amount)
    (cond [(equal? direction "R") (modulo (+ position amount) 100)]
          [(equal? direction "L") (modulo (- position amount) 100)]))

(define (process-move position move)
  (let* ([direction (substring move 0 1)]
         [amount (string->number (substring move 1))])
    (update-position position direction amount)))

(define (count-safe-moves moves n)
  (let* ([position 50]
         [n-count 0])
    (for-each
      (lambda (move)
        (set! position (process-move position move))
        (when (= position n)
          (set! n-count (+ n-count 1))))
    moves)
  n-count))
