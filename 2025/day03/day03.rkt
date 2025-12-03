#lang racket

(require racket/list)

(provide sum-highest-joltage-pairs-of
         largest-joltage-of
         sum-largest-joltage-of)

; Shared helpers
(define (string->digit-vector s)
  (list->vector
   (map (lambda (c) (- (char->integer c) (char->integer #\0)))
        (string->list s))))

(define (pop-while stack drops d)
  (cond
    [(and (> drops 0) (pair? stack) (< (car stack) d))
     (pop-while (cdr stack) (sub1 drops) d)]
    [else (values stack drops)]))

(define (max-subsequence-number vec k)
  (let* ([n (vector-length vec)]
         [keep (min k n)]
         [drops (- n keep)])
    
    (define-values (stack _)
      (for/fold ([stk '()] [drp drops]) ([d (in-vector vec)])
        (define-values (stk* drp*) (pop-while stk drp d))
        (values (cons d stk*) drp*)))

    (define picked (take (reverse stack) keep))
    
    (for/fold ([acc 0]) ([d picked])
      (+ (* acc 10) d))))

; Part 1
(define (highest-joltages-of s)
  (max-subsequence-number (string->digit-vector s) 2))

(define (sum-highest-joltage-pairs-of input-list)
  (apply + (map highest-joltages-of input-list)))

; Part 2
(define (largest-joltage-of s)
  (max-subsequence-number (string->digit-vector s) 12))

(define (sum-largest-joltage-of input-list)
  (apply + (map largest-joltage-of input-list)))
