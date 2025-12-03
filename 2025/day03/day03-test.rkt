#lang racket

(require "day03.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define input (file->lines "input.txt"))
  
  (define suite
    (test-suite "Bank calculations"
      (test-eq? "Can solve 987654321111111 for Part 1"
                (sum-highest-joltage-pairs-of '("987654321111111"))
                98)

      (test-eq? "Can solve 811111111111119 for Part 1"
                (sum-highest-joltage-pairs-of '("811111111111119"))
                89)

      (test-eq? "Can solve 234234234234278 for Part 1"
                (sum-highest-joltage-pairs-of '("234234234234278"))
                78)

      (test-eq? "Can solve 818181911112111 for Part 1"
                (sum-highest-joltage-pairs-of '("818181911112111"))
                92)

      (test-eq? "Can solve sample case for Part 1"
                (sum-highest-joltage-pairs-of '("987654321111111" "811111111111119" "234234234234278" "818181911112111"))
                357)

      (test-eq? "Can find the correct result from input.txt for Part 1"
                (sum-highest-joltage-pairs-of input)
                16993)

      (test-eq? "Can solve 987654321111111 for Part 2"
                (largest-joltage-of "987654321111111")
                987654321111)

      (test-eq? "Can solve 811111111111119 for Part 2"
                (largest-joltage-of "811111111111119")
                811111111119)

      (test-eq? "Can solve 234234234234278 for Part 2"
                (largest-joltage-of "234234234234278")
                434234234278)

      (test-eq? "Can solve 818181911112111 for Part 2"
                (largest-joltage-of "818181911112111")
                888911112111)

      (test-eq? "Can solve sample case for Part 2"
                (sum-largest-joltage-of '("987654321111111" "811111111111119" "234234234234278" "818181911112111"))
                3121910778619)

      (test-eq? "Can find the correct result from input.txt for Part 2"
                (sum-largest-joltage-of input)
                168617068915447)))
  (run-tests suite))
