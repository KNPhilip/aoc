#lang racket

(require "day04.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define input (file->lines "input.txt"))
  (define sample
  '("..@@.@@@@."
    "@@@.@.@.@@"
    "@@@@@.@.@@"
    "@.@@@@..@."
    "@@.@@@@.@@"
    ".@@@@@@@.@"
    ".@.@.@.@@@"
    "@.@@@.@@@@"
    ".@@@@@@@@."
    "@.@.@@@.@."))
  
  (define suite
    (test-suite "Calculations of paper roll locations"
      (test-eq? "Can solve sample case for Part 1"
                (count-accessible-rolls sample)
                13)

      (test-eq? "Can find the correct result from input.txt for Part 1"
                (count-accessible-rolls input)
                1553)

      (test-eq? "Can solve sample case for Part 2"
                (total-removable-rolls sample)
                43)

      (test-eq? "Can find the correct result from input.txt for Part 2"
                (total-removable-rolls input)
                8442)))
  (run-tests suite))
