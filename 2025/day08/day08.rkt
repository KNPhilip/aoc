#lang racket

(require racket/string
         racket/list)

(provide solve-playground)

(define (xyz-lines->vector lines)
  (list->vector
    (for/list ([ln (in-list lines)]
               #:unless (regexp-match? #px"^\\s*$" ln))
      (let* ([parts (map string-trim (string-split ln ","))]
             [x (string->number (list-ref parts 0))]
             [y (string->number (list-ref parts 1))]
             [z (string->number (list-ref parts 2))])
        (vector x y z)))))

;; dist2 => Squared Euclidean distance between points i and j
(define (dist2 pts i j)
  (let* ([pi (vector-ref pts i)]
         [pj (vector-ref pts j)]
         [dx (- (vector-ref pi 0) (vector-ref pj 0))]
         [dy (- (vector-ref pi 1) (vector-ref pj 1))]
         [dz (- (vector-ref pi 2) (vector-ref pj 2))])
    (+ (* dx dx) (* dy dy) (* dz dz))))

;; DSU => Disjoint-set union (union-find) with sizes
(define (make-dsu n)
  (values (build-vector n values) (make-vector n 1)))

(define (dsu-find! parent x)
  (let ([p (vector-ref parent x)])
    (if (= p x) x
        (let ([r (dsu-find! parent p)])
          (vector-set! parent x r)
          r))))

;; Return #t iff merged (different sets), #f if already same set
(define (dsu-union! parent sz a b)
  (let ([ra (dsu-find! parent a)]
        [rb (dsu-find! parent b)])
    (if (= ra rb)
        #f
        (begin
          (when (< (vector-ref sz ra) (vector-ref sz rb))
            (let ([t ra]) (set! ra rb) (set! rb t)))
          (vector-set! parent rb ra)
          (vector-set! sz ra (+ (vector-ref sz ra) (vector-ref sz rb)))
          #t))))

;; Keep exactly k globally smallest pairs using a bounded max-heap (implemented inline).
;; Each record: #(d i j) ordered by (d, i, j).
(define (k-smallest-pairs pts k)
  (let* ([n (vector-length pts)]
         [total (quotient (* n (sub1 n)) 2)]
         [k* (min k total)]
         [cap (max 1 k*)]
         [vec (make-vector cap #f)]
         [size 0])
    (define (pair> a b)
      (define da (vector-ref a 0)) (define db (vector-ref b 0))
      (cond [(> da db) #t]
            [(< da db) #f]
            [else (define ia (vector-ref a 1)) (define ib (vector-ref b 1))
                  (cond [(> ia ib) #t]
                        [(< ia ib) #f]
                        [else (> (vector-ref a 2) (vector-ref b 2))])]))
    (define (swap! i j)
      (define t (vector-ref vec i))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j t))
    (define (sift-up i)
      (let loop ([k i])
        (unless (zero? k)
          (define p (quotient (sub1 k) 2))
          (when (pair> (vector-ref vec k) (vector-ref vec p))
            (swap! k p)
            (loop p)))))
    (define (sift-down i)
      (let loop ([k i])
        (let* ([L (+ (* 2 k) 1)]
               [R (+ L 1)]
               [largest (cond [(and (< L size) (pair> (vector-ref vec L) (vector-ref vec k))) L]
                              [else k])]
               [largest2 (cond [(and (< R size) (pair> (vector-ref vec R) (vector-ref vec largest))) R]
                               [else largest])])
        (when (not (= largest2 k))
          (swap! k largest2)
          (loop largest2)))))
    (define (insert! x)
      (cond
        [(< size cap)
         (vector-set! vec size x)
         (set! size (add1 size))
         (sift-up (sub1 size))]
        [else
         (when (pair> (vector-ref vec 0) x)
           (vector-set! vec 0 x)
           (sift-down 0))]))
    (for* ([i (in-range n)]
           [j (in-range (add1 i) n)])
      (insert! (vector (dist2 pts i j) i j)))
    (define pairs (for/list ([idx (in-range size)]) (vector-ref vec idx)))
    (define (pair< a b)
      (define da (vector-ref a 0))
      (define db (vector-ref b 0))
      (cond [(< da db) #t]
            [(> da db) #f]
            [else (define ia (vector-ref a 1)) (define ib (vector-ref b 1))
                  (cond [(< ia ib) #t]
                        [(> ia ib) #f]
                        [else (< (vector-ref a 2) (vector-ref b 2))])]))
    (sort pairs pair<)))

;; Multiply sizes of the three largest components (pad with 1s if fewer than 3)
(define (product-top3 parent sz)
  (let* ([n (vector-length parent)]
         [sizes (for/list ([i (in-range n)])
                  (define r (dsu-find! parent i))
                  r)]
         [size-table (make-hash)])
    (for ([r (in-list sizes)]) (hash-update! size-table r add1 0))
    (apply * (take (append (sort (hash-values size-table) >) '(1 1 1)) 3))))

;; Process exactly k connections (by globally smallest pairs)
(define (solve-playground lines connections)
  (let* ([pts (xyz-lines->vector lines)]
         [n (vector-length pts)]
         [pairs (k-smallest-pairs pts connections)])
    (define-values (parent sz) (make-dsu n))
    (for ([p (in-list pairs)])
      (dsu-union! parent sz (vector-ref p 1) (vector-ref p 2)))
    (product-top3 parent sz)))