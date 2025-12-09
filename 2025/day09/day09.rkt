#lang racket

(require racket/string
         rackunit)

(provide largest-rectangle-area
         largest-rectangle-area-green)

;; Shared
(define (parse-coordinates input)
  (map (lambda (line)
         (let* ([parts (string-split line ",")]
                [x (string->number (first parts))]
                [y (string->number (second parts))])
           (cons x y)))
       input))

(define (calculate-area p1 p2)
  (let* ([x1 (car p1)] [y1 (cdr p1)]
         [x2 (car p2)] [y2 (cdr p2)]
         [w (+ 1 (abs (- x2 x1)))]
         [h (+ 1 (abs (- y2 y1)))])
    (* w h)))

;; Part 1
(define (largest-rectangle-area lines)
  (let* ([coords (parse-coordinates lines)]
         [n (length coords)]
         [max-area 0])
    (for* ([i (in-range n)]
           [j (in-range (add1 i) n)])
      (let* ([p1 (list-ref coords i)]
             [p2 (list-ref coords j)]
             [a (calculate-area p1 p2)])
        (set! max-area (max max-area a))))
    max-area))

;; Part 2
(define (build-edges coords)
  (let* ([n (length coords)]
         [hs '()]
         [vs '()])
    (for ([i (in-range n)])
      (let* ([p1 (list-ref coords i)]
             [p2 (list-ref coords (modulo (add1 i) n))]
             [x1 (car p1)] [y1 (cdr p1)]
             [x2 (car p2)] [y2 (cdr p2)]
             [lo (min y1 y2)] [hi (max y1 y2)])
        (cond
          [(= x1 x2) (set! vs (cons (list x1 lo hi) vs))]
          [(= y1 y2) (set! hs (cons (list y1 (min x1 x2) (max x1 x2)) hs))])))
    (values hs vs)))

(define (point-on-boundary? hs vs x y)
  (or (for/or ([e hs]) (apply (lambda (ey xmin xmax) (and (= y ey) (<= xmin x xmax))) e))
      (for/or ([e vs]) (apply (lambda (ex ymin ymax) (and (= x ex) (<= ymin y ymax))) e))))

(define (inside-or-on? hs vs x y)
  (or (point-on-boundary? hs vs x y)
      (let* ([cnt (for/sum ([e vs])
                    (let* ([ex (first e)] [ymin (second e)] [ymax (third e)])
                      (if (and (> ex x) (<= ymin y) (< y ymax)) 1 0)))])
        (odd? cnt))))

(define (open-overlap? a b c d)
  (> (min b d) (max a c)))

(define (edges-hit-rect-interior? hs vs minx maxx miny maxy)
  (or (for/or ([e hs]) (apply (lambda (ey xmin xmax) (and (> ey miny) (< ey maxy) (open-overlap? minx maxx xmin xmax))) e))
      (for/or ([e vs]) (apply (lambda (ex ymin ymax) (and (> ex minx) (< ex maxx) (open-overlap? miny maxy ymin ymax))) e))))

(define (vertical-segment-allowed? hs vs x y1 y2)
  (let* ([lo (min y1 y2)] [hi (max y1 y2)])
    (cond
      [(= lo hi) (inside-or-on? hs vs x lo)]
      [(for/or ([e vs])
         (let* ([ex (first e)] [ymin (second e)] [ymax (third e)])
           (and (= x ex) (<= ymin lo) (<= hi ymax))))
       #t]
      [else
       (let* ([ys (sort
                   (for/list ([e hs] #:when (let* ([ey (first e)]
                                                   [xmin (second e)] [xmax (third e)])
                                              (and (<= xmin x) (<= x xmax))))
                     (first e))
                   <)]
              [status (inside-or-on? hs vs x (+ lo 0.5))]
              [prev lo]
              [ok #t])
         (for ([yy ys] #:when (< lo yy hi))
           (when (and (not status) (> yy prev)) (set! ok #f))
           (set! status (not status))
           (set! prev yy))
         (when (and (not status) (> hi prev)) (set! ok #f))
         (and ok (inside-or-on? hs vs x lo) (inside-or-on? hs vs x hi)))]
      )))

(define (horizontal-segment-allowed? hs vs y x1 x2)
  (let* ([lo (min x1 x2)] [hi (max x1 x2)])
    (cond
      [(= lo hi) (inside-or-on? hs vs lo y)]
      [(for/or ([e hs]) (apply (lambda (ey xmin xmax) (and (= y ey) (<= xmin lo hi xmax))) e)) #t]
      [else
       (let* ([xs (sort (for/list ([e vs] #:when (apply (lambda (ex ymin ymax) (and (<= ymin y ymax)) e))) (first e)) <)]
              [status (inside-or-on? hs vs (+ lo 0.5) y)]
              [prev lo] [ok #t])
         (for ([xx xs] #:when (< lo xx hi))
           (when (and (not status) (> xx prev)) (set! ok #f))
           (set! status (not status))
           (set! prev xx))
         (when (and (not status) (> hi prev)) (set! ok #f))
         (and ok (inside-or-on? hs vs lo y) (inside-or-on? hs vs hi y)))]
      )))

(define (rect-allowed? hs vs p1 p2)
  (let* ([x1 (car p1)] [y1 (cdr p1)]
         [x2 (car p2)] [y2 (cdr p2)]
         [minx (min x1 x2)] [maxx (max x1 x2)]
         [miny (min y1 y2)] [maxy (max y1 y2)])
    (cond
      [(= minx maxx) (vertical-segment-allowed? hs vs minx miny maxy)]
      [(= miny maxy) (horizontal-segment-allowed? hs vs miny minx maxx)]
      [else
       (let* ([cx (/ (+ minx maxx) 2.0)] [cy (/ (+ miny maxy) 2.0)])
         (and (inside-or-on? hs vs cx cy)
              (not (edges-hit-rect-interior? hs vs minx maxx miny maxy))))
      ])))

(define (largest-rectangle-area-green lines)
  (let* ([coords (parse-coordinates lines)]
         [n (length coords)]
         [max-area 0])
    (define-values (hs vs) (build-edges coords))
    (for* ([i (in-range n)] [j (in-range (add1 i) n)])
      (let* ([p1 (list-ref coords i)]
             [p2 (list-ref coords j)]
             [a  (calculate-area p1 p2)])
        (when (rect-allowed? hs vs p1 p2)
          (set! max-area (max max-area a)))))
    max-area))

;; --- Note ---
;;
;; The above code is horrible. It's slow, inefficient, and kind of a mess to read.
;; But! It works. And right now I can't figure out how to do it in a less complicated way..
;;
;; I asked GPT-5 (High Reasoning) to refactor the above and make it fast, efficient, and clean.
;; The below outcommented code is what came out of that session.
;; 
;; Needlesss to say, I did not use it.
;;

;; Part 2 (optimized)
;;
;;(define (build-edges coords)
;;  (let* ([n (length coords)]
;;         [hs '()]
;;         [vs '()])
;;    (for ([i (in-range n)])
;;      (let* ([p1 (list-ref coords i)]
;;             [p2 (list-ref coords (modulo (add1 i) n))]
;;             [x1 (car p1)] [y1 (cdr p1)]
;;             [x2 (car p2)] [y2 (cdr p2)]
;;             [lo (min y1 y2)] [hi (max y1 y2)])
;;        (cond
;;          [(= x1 x2) (set! vs (cons (list x1 lo hi) vs))]
;;          [(= y1 y2) (set! hs (cons (list y1 (min x1 x2) (max x1 x2)) hs))])))
;;    (values hs vs)))
;;
;;(define (largest-rectangle-area-green lines)
;;  (let* ([coords (parse-coordinates lines)]
;;         [n (length coords)]
;;         [max-area 0])
;;    (define-values (hs vs) (build-edges coords))
;;
;;    (define hx (make-hash))
;;    (define vx (make-hash))
;;    (for ([e hs])
;;      (let* ([y (first e)] [a (second e)] [b (third e)])
;;        (hash-set! hx y (cons (cons a b) (hash-ref hx y '())))))
;;    (for ([e vs])
;;      (let* ([x (first e)] [a (second e)] [b (third e)])
;;        (hash-set! vx x (cons (cons a b) (hash-ref vx x '())))))
;;
;;    (define (sort-intervals! h)
;;      (for ([(k v) (in-hash h)])
;;        (hash-set! h k (list->vector (sort v (lambda (p q) (< (car p) (car q))))))))
;;    (sort-intervals! hx)
;;    (sort-intervals! vx)
;;
;;    (define hx-ys (list->vector (sort (hash-keys hx) <)))
;;    (define vx-xs (list->vector (sort (hash-keys vx) <)))
;;
;;    (define ys-x-cache (make-hash))
;;    (define xs-y-cache (make-hash))
;;    (define xs-yh-cache (make-hash))
;;
;;    (define (normalize-int v)
;;      (if (and (integer? v) (inexact? v)) (inexact->exact v) v))
;;
;;    (define (interval-contains? vec v)
;;      (for/or ([iv (in-vector vec)])
;;        (let* ([a (car iv)] [b (cdr iv)])
;;          (and (<= a v) (<= v b)))))
;;
;;    (define (point-on-boundary? x y)
;;      (let* ([x (normalize-int x)] [y (normalize-int y)]
;;             [hints (hash-ref hx y #f)]
;;             [vints (hash-ref vx x #f)])
;;        (or (and hints (interval-contains? hints x))
;;            (and vints (interval-contains? vints y)))))
;;
;;    (define (ys-at-x x)
;;      (let* ([x (normalize-int x)]
;;             [cached (hash-ref ys-x-cache x #f)])
;;        (if cached
;;            cached
;;            (let* ([acc '()])
;;              (for ([(y ivs) (in-hash hx)])
;;                (for ([iv (in-vector ivs)])
;;                  (let ([a (car iv)] [b (cdr iv)])
;;                    (when (and (<= a x) (<= x b))
;;                      (set! acc (cons y acc))))))
;;              (let ([vec (list->vector (sort acc <))])
;;                (hash-set! ys-x-cache x vec)
;;                vec)))))
;;
;;    (define (xs-at-y-incl y)
;;      (let* ([y (normalize-int y)]
;;             [cached (hash-ref xs-y-cache y #f)])
;;        (if cached
;;            cached
;;            (let* ([acc '()])
;;              (for ([e vs])
;;                (let* ([x (first e)] [a (second e)] [b (third e)])
;;                  (when (and (<= a y) (<= y b)) (set! acc (cons x acc)))))
;;              (let ([vec (list->vector (sort acc <))])
;;                (hash-set! xs-y-cache y vec)
;;                vec)))))
;;
;;    (define (xs-at-y-half y)
;;      (let* ([cached (hash-ref xs-yh-cache y #f)])
;;        (if cached
;;            cached
;;            (let* ([acc '()])
;;              (for ([e vs])
;;                (let* ([x (first e)] [a (second e)] [b (third e)])
;;                  (when (and (<= a y) (< y b)) (set! acc (cons x acc)))))
;;              (let ([vec (list->vector (sort acc <))])
;;                (hash-set! xs-yh-cache y vec)
;;                vec)))))
;;
;;    (define (bisect-right vec x)
;;      (let loop ([lo 0] [hi (vector-length vec)])
;;        (if (>= lo hi)
;;            lo
;;            (let* ([mid (quotient (+ lo hi) 2)]
;;                   [v (vector-ref vec mid)])
;;              (if (<= v x)
;;                  (loop (add1 mid) hi)
;;                  (loop lo mid))))))
;;
;;    (define (inside-or-on? x y)
;;      (if (point-on-boundary? x y)
;;          #t
;;          (let* ([use-cache (and (integer? y) (exact? y))]
;;                 [xs (if use-cache (xs-at-y-half y) #f)])
;;            (if xs
;;                (let* ([idx (bisect-right xs x)]
;;                       [cnt (- (vector-length xs) idx)])
;;                  (odd? cnt))
;;                (let* ([cnt (for/sum ([e vs])
;;                              (let* ([ex (first e)] [a (second e)] [b (third e)])
;;                                (if (and (> ex x) (<= a y) (< y b)) 1 0)))])
;;                  (odd? cnt))))))
;;
;;    (define (vertical-segment-allowed? x y1 y2)
;;      (let* ([lo (min y1 y2)] [hi (max y1 y2)])
;;        (cond
;;          [(= lo hi) (inside-or-on? x lo)]
;;          [else
;;           (let* ([vints (hash-ref vx (normalize-int x) #f)])
;;             (cond
;;               [(and vints (for/or ([iv (in-vector vints)])
;;                             (let ([a (car iv)] [b (cdr iv)])
;;                               (and (<= a lo) (<= hi b))))) #t]
;;               [else
;;                (let* ([ys (ys-at-x x)]
;;                       [status (inside-or-on? x (+ lo 0.5))]
;;                       [prev lo]
;;                       [ok #t])
;;                  (for ([yy (in-vector ys)])
;;                    (when (< lo yy hi)
;;                      (when (and (not status) (> yy prev)) (set! ok #f))
;;                      (set! status (not status))
;;                      (set! prev yy)))
;;                  (when (and (not status) (> hi prev)) (set! ok #f))
;;                  (and ok (inside-or-on? x lo) (inside-or-on? x hi)))]))])))
;;
;;    (define (horizontal-segment-allowed? y x1 x2)
;;      (let* ([lo (min x1 x2)] [hi (max x1 x2)])
;;        (cond
;;          [(= lo hi) (inside-or-on? lo y)]
;;          [else
;;           (let* ([hints (hash-ref hx (normalize-int y) #f)])
;;             (cond
;;               [(and hints (for/or ([iv (in-vector hints)])
;;                             (let ([a (car iv)] [b (cdr iv)])
;;                               (and (<= a lo) (<= hi b))))) #t]
;;               [else
;;                (let* ([xs (xs-at-y-incl y)]
;;                       [status (inside-or-on? (+ lo 0.5) y)]
;;                       [prev lo]
;;                       [ok #t])
;;                  (for ([xx (in-vector xs)])
;;                    (when (< lo xx hi)
;;                      (when (and (not status) (> xx prev)) (set! ok #f))
;;                      (set! status (not status))
;;                      (set! prev xx)))
;;                  (when (and (not status) (> hi prev)) (set! ok #f))
;;                  (and ok (inside-or-on? lo y) (inside-or-on? hi y)))]))])))
;;
;;    (define (open-overlap? a b c d)
;;      (> (min b d) (max a c)))
;;
;;    (define (range-keys vec lo hi)
;;      (let loopi ([l 0] [r (vector-length vec)])
;;        (if (>= l r)
;;            (let loopj ([l l] [r r])
;;              (values l l))
;;            (let* ([m (quotient (+ l r) 2)]
;;                   [v (vector-ref vec m)])
;;              (if (<= v lo)
;;                  (loopi (add1 m) r)
;;                  (let loopj ([l l] [r (vector-length vec)])
;;                    (if (>= l r)
;;                        (values (sub1 m) l) ; placeholder, we’ll redo below
;;                        (values 0 0)))))))
;;      (let rec ([l 0] [r (vector-length vec)] [p #f])
;;        (if (>= l r)
;;            (let rec2 ([l l] [r (vector-length vec)])
;;              (if (>= l r)
;;                  (values (or p 0) l)
;;                  (let* ([m (quotient (+ l r) 2)]
;;                         [v (vector-ref vec m)])
;;                    (if (< v hi)
;;                        (rec2 (add1 m) r)
;;                        (rec2 l m)))))
;;            (let* ([m (quotient (+ l r) 2)]
;;                   [v (vector-ref vec m)])
;;              (if (<= v lo)
;;                  (rec (add1 m) r #f)
;;                  (rec l m m)))))
;;
;;    (define (edges-hit-rect-interior? minx maxx miny maxy)
;;      (let-values ([(i j) (let loop ([l 0] [r (vector-length hx-ys)])
;;                            (if (>= l r)
;;                                (let loop2 ([l l] [r (vector-length hx-ys)])
;;                                  (if (>= l r)
;;                                      (values l l)
;;                                      (let* ([m (quotient (+ l r) 2)]
;;                                             [v (vector-ref hx-ys m)])
;;                                        (if (< v maxy)
;;                                            (loop2 (add1 m) r)
;;                                            (loop2 l m)))))
;;                                (let* ([m (quotient (+ l r) 2)]
;;                                       [v (vector-ref hx-ys m)])
;;                                  (if (<= v miny)
;;                                      (loop (add1 m) r)
;;                                      (loop l m)))))])
;;        (define hit-h
;;          (let loop ([idx i])
;;            (if (>= idx j) #f
;;                (let* ([yy (vector-ref hx-ys idx)]
;;                       [ivs (hash-ref hx yy)]
;;                       [hit (for/or ([iv (in-vector ivs)])
;;                              (let ([a (car iv)] [b (cdr iv)])
;;                                (and (> yy miny) (< yy maxy) (open-overlap? minx maxx a b))))])
;;                  (or hit (loop (add1 idx)))))))
;;        (if hit-h
;;            #t
;;            (let-values ([(i j) (let loop ([l 0] [r (vector-length vx-xs)])
;;                                  (if (>= l r)
;;                                      (let loop2 ([l l] [r (vector-length vx-xs)])
;;                                        (if (>= l r)
;;                                            (values l l)
;;                                            (let* ([m (quotient (+ l r) 2)]
;;                                                   [v (vector-ref vx-xs m)])
;;                                              (if (< v maxx)
;;                                                  (loop2 (add1 m) r)
;;                                                  (loop2 l m)))))
;;                                      (let* ([m (quotient (+ l r) 2)]
;;                                             [v (vector-ref vx-xs m)])
;;                                        (if (<= v minx)
;;                                            (loop (add1 m) r)
;;                                            (loop l m)))))])
;;              (let loop ([idx i])
;;                (if (>= idx j) #f
;;                    (let* ([xx (vector-ref vx-xs idx)]
;;                           [ivs (hash-ref vx xx)]
;;                           [hit (for/or ([iv (in-vector ivs)])
;;                                  (let ([a (car iv)] [b (cdr iv)])
;;                                    (and (> xx minx) (< xx maxx) (open-overlap? miny maxy a b))))])
;;                      (or hit (loop (add1 idx))))))))))
;;
;;    (define (rect-allowed? p1 p2)
;;      (let* ([x1 (car p1)] [y1 (cdr p1)]
;;             [x2 (car p2)] [y2 (cdr p2)]
;;             [minx (min x1 x2)] [maxx (max x1 x2)]
;;             [miny (min y1 y2)] [maxy (max y1 y2)])
;;        (cond
;;          [(= minx maxx) (vertical-segment-allowed? minx miny maxy)]
;;          [(= miny maxy) (horizontal-segment-allowed? miny minx maxx)]
;;          [else
;;           (let* ([cx (/ (+ minx maxx) 2.0)]
;;                  [cy (/ (+ miny maxy) 2.0)])
;;             (and (inside-or-on? cx cy)
;;                  (not (edges-hit-rect-interior? minx maxx miny maxy))))])))
;;
;;    (define coords-v (list->vector coords))
;;    (for* ([i (in-range n)] [j (in-range (add1 i) n)])
;;      (let* ([p1 (vector-ref coords-v i)]
;;             [p2 (vector-ref coords-v j)]
;;             [a (calculate-area p1 p2)])
;;        (when (rect-allowed? p1 p2)
;;          (set! max-area (max max-area a)))))
;;    max-area))
;;