#lang racket

(require "day10.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define sample '(
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
    "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
    "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"))
  
  (define input (file->lines "input.txt"))

  (define suite
    (test-suite "Re-enabling the Factory Test Suite"
      (test-case "Single machine: one button toggles correct light"
        (check-equal? (fewest-button-presses '("[#] (0) {}")) 1))

      (test-case "Single machine: two buttons needed together"
        (check-equal? (fewest-button-presses '("[##] (0) (1) {}")) 2))

      (test-case "Multiple machines sum results"
        (check-equal? (fewest-button-presses '(
          "[#] (0) {}"      ; needs 1
          "[##] (0) (1) {}" ; needs 2
          "[.] () {}"       ; needs 0
     )) (+ 1 2 0)))

      (test-case "Machine with redundant buttons"
        (check-equal? (fewest-button-presses '("[#.] (0) (0) (0) (0) {}")) 1))
                
      (test-case "Can solve sample case for Part 1"
        (check-equal? (fewest-button-presses sample) 7))

      (test-case "Can find the correct result from input.txt for Part 1"
        (check-equal? (fewest-button-presses input) 558))))
  (run-tests suite))
