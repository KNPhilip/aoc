#lang racket

(provide largest-rectangle-area)

(define (parse-coordinates input)
  (map (lambda (line)
         (let* ([parts (string-split line ",")]
                [x (string->number (first parts))]
                [y (string->number (second parts))])
           (cons x y)))
       input))

(define (calculate-area p1 p2)
  (let* ([x1 (car p1)]
         [y1 (cdr p1)]
         [x2 (car p2)]
         [y2 (cdr p2)]
         [width (+ 1 (abs (- x2 x1)))]
         [height (+ 1 (abs (- y2 y1)))])
    (* width height)))

(define (largest-rectangle-area input)
  (let* ([coords (parse-coordinates input)]
         [num-red-tiles (length coords)]
         [max-area 0])
    (for ([i (in-range num-red-tiles)])
      (for ([j (in-range (add1 i) num-red-tiles)])
        (let* ([p1 (list-ref coords i)]
               [p2 (list-ref coords j)]
               [area (calculate-area p1 p2)])
          (set! max-area (max max-area area)))))
    max-area))