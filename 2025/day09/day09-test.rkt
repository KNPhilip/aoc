#lang racket

(require "day09.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define sample '("7,1" "11,1" "11,7" "9,7" "9,5" "2,5" "2,3" "7,3"))
  (define input (file->lines "input.txt"))

  (define suite
    (test-suite "Movie Theater Test Suite"
      (test-case "Can solve sample case for Part 1"
        (check-equal? (largest-rectangle-area sample) 50))

      (test-case "Can find the correct result from input.txt for Part 1"
        (check-equal? (largest-rectangle-area input) 4737096935))

      (test-case "Can solve sample case for Part 2"
        (check-equal? (largest-rectangle-area-green sample) 24))

      (test-case "Can find the correct result from input.txt for Part 2"
        (check-equal? (largest-rectangle-area-green input) 1644094530))))
  (run-tests suite))
