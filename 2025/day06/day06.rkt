#lang racket

(require racket/list)

(provide compute-total
         compute-total-2)

(define (text->grid text)
  (let* ([lines0 (regexp-split #px"\r?\n" text)]
         [lines
          (let loop ([rs (reverse lines0)])
            (if (and (pair? rs) (string=? (car rs) "")) (loop (cdr rs)) (reverse rs)))]
         [w (apply max 0 (map string-length lines))])
    (map (λ (s) (string-append s (make-string (- w (string-length s)) #\space))) lines)))

(define (column-ranges grid)
  (let* ([w (string-length (car grid))]
         [space-col? (λ (j) (for/and ([row grid]) (char=? (string-ref row j) #\space)))])
    (let loop ([j 0] [in? #f] [start 0] [acc '()])
      (cond
        [(= j w) (reverse (if in? (cons (cons start w) acc) acc))]
        [(space-col? j)
         (loop (add1 j) #f start (if in? (cons (cons start j) acc) acc))]
        [else (loop (add1 j) #t (if in? start j) acc)]))))

(define (nums-by-rows grid start end)
  (let* ([h (length grid)]
         [top (take grid (sub1 h))]
         [seg (λ (s) (substring s start end))])
    (filter values
            (for/list ([row top])
              (let ([ds (regexp-replace* #px"\\D" (seg row) "")])
                (and (positive? (string-length ds)) (string->number ds)))))
    ))

(define (nums-by-cols grid start end)
  (let* ([h (length grid)]
         [top (take grid (sub1 h))])
    (filter values
            (for/list ([j (in-range (sub1 end) (sub1 start) -1)])
              (let ([ds (list->string
                         (for/list ([row top]
                                    #:when (char-numeric? (string-ref row j)))
                           (string-ref row j)))])
                (and (positive? (string-length ds)) (string->number ds)))))
    ))

(define (op-in-block grid start end)
  (let* ([bottom (list-ref grid (sub1 (length grid)))]
         [seg (substring bottom start end)]
         [m (regexp-match #rx"[+*]" seg)])
    (unless m (error 'op-in-block "no operator in block ~a-~a" start end))
    (string-ref (car m) 0)))

(define (solve-with text nums-extractor)
  (let ([grid (text->grid text)])
    (if (null? grid) 0
        (for/sum ([rg (column-ranges grid)])
          (let* ([start (car rg)]
                 [end   (cdr rg)]
                 [nums  (nums-extractor grid start end)]
                 [op    (op-in-block grid start end)])
            (when (null? nums)
              (error 'solve "block ~a-~a has no numbers" start end))
            (if (char=? op #\+) (apply + nums) (apply * nums))))
        )))

(define (compute-total text)
  (solve-with text nums-by-rows))

(define (compute-total-2 text)
  (solve-with text nums-by-cols))