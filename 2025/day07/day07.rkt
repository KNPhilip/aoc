#lang racket

(require racket/string)

(provide solve-laboratories)

(define (lines->grid lines)
  (let ([w (apply max 0 (map string-length lines))])
    (map (λ (s) (string-append s
           (make-string (- w (string-length s)) #\space)))
         lines)))

(define (text->grid text)
  (lines->grid (regexp-split #px"\r?\n" text)))

(define (input->grid input)
  (cond [(string? input) (text->grid input)]
        [(and (list? input) (andmap string? input)) (lines->grid input)]
        [else (error 'input->grid "expected string or list of strings; got ~v" input)]))

(define (find-start grid)
  (let* ([h (length grid)]
         [w (if (zero? h) 0 (string-length (car grid)))])
    (let loop-r ([r 0])
      (if (= r h) (error 'find-start "no S found")
          (let ([row (list-ref grid r)])
            (let loop-c ([c 0])
              (cond [(= c w) (loop-r (add1 r))]
                    [(char=? (string-ref row c) #\S) (values r c)]
                    [else (loop-c (add1 c))])))
          ))))

(define (step-row-bool row beams)
  (let* ([w (vector-length beams)]
         [next (make-vector w #f)])
    (let loop ([c 0] [splits 0])
      (if (= c w)
          (values next splits)
          (let ([present (vector-ref beams c)])
            (if present
                (let ([ch (string-ref row c)])
                  (if (char=? ch #\^)
                      (begin
                        (when (> c 0)        (vector-set! next (sub1 c) #t))
                        (when (< c (sub1 w)) (vector-set! next (add1 c) #t))
                        (loop (add1 c) (add1 splits)))
                      (begin
                        (vector-set! next c #t)
                        (loop (add1 c) splits))))
                (loop (add1 c) splits))))
      )))

(define (solve-laboratories input)
  (let* ([grid (input->grid input)]
         [h    (length grid)]
         [w    (if (zero? h) 0 (string-length (car grid)))])
    (when (zero? h) (error 'solve-laboratories "empty grid"))
    (let-values ([(sr sc) (find-start grid)])
      (let loop ([r (add1 sr)]
                 [beams (let ([v (make-vector w #f)]) (vector-set! v sc #t) v)]
                 [acc 0])
        (if (= r h)
            acc
            (let-values ([(nb s) (step-row-bool (list-ref grid r) beams)])
              (loop (add1 r) nb (+ acc s))))
        ))))
