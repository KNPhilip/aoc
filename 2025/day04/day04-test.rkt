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
    (test-suite "Bank calculations"
      (test-eq? "Can solve sample case for Part 1"
                (count-accessible-rolls sample)
                13)

      (test-eq? "Can find the correct result from input.txt for Part 1"
                (count-accessible-rolls input)
                1553)))
  (run-tests suite))
