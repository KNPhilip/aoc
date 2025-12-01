#lang racket

(provide count-safe-moves
         count-safe-moves-part2)

; Part 1
(define (update-position position direction amount)
    (cond [(equal? direction "R") (modulo (+ position amount) 100)]
          [(equal? direction "L") (modulo (- position amount) 100)]))

(define (process-move position move)
  (let ([direction (substring move 0 1)]
         [amount (string->number (substring move 1))])
    (update-position position direction amount)))

(define (count-safe-moves moves n)
  (let ([position 50]
         [n-count 0])
    (for-each
      (lambda (move)
        (set! position (process-move position move))
        (when (= position n)
          (set! n-count (add1 n-count))))
    moves)
  n-count))

; Part 2
(define (update-pos pos direction)
  (cond [(equal? direction "L") (modulo (+ (- pos 1) 100) 100)]
        [(equal? direction "R") (modulo (+ pos 1) 100)]))

(define (count-safe-moves-part2 moves)
  (let ([position 50]
        [passes 0])
    
    (for-each
      (lambda (move)
        (let ([direction (substring move 0 1)]
              [amount (string->number (substring move 1))])

        (for ([_ amount])
          (set! position (update-pos position direction))
          (when (zero? position)
            (set! passes (add1 passes)))
      )))
    moves)
  passes))
