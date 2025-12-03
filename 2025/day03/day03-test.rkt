#lang racket

(require "day03.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define input (file->lines "input.txt"))
  
  (define suite
    (test-suite "Bank calculations"
      (test-eq? "Can solve 987654321111111"
                (sum-highest-joltage-pairs-of '("987654321111111"))
                98)

      (test-eq? "Can solve 811111111111119"
                (sum-highest-joltage-pairs-of '("811111111111119"))
                89)

      (test-eq? "Can solve 234234234234278"
                (sum-highest-joltage-pairs-of '("234234234234278"))
                78)

      (test-eq? "Can solve 818181911112111"
                (sum-highest-joltage-pairs-of '("818181911112111"))
                92)

      (test-eq? "Can solve sample case for Part 1"
                (sum-highest-joltage-pairs-of '("987654321111111" "811111111111119" "234234234234278" "818181911112111"))
                357)

      (test-eq? "Can find the correct result from input.txt for Part 1"
                (sum-highest-joltage-pairs-of input)
                16993)


      ))

  (run-tests suite))
