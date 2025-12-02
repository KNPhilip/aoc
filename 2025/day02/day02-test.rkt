#lang racket

(require "day02.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define sample "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
  (define input (file->string "input.txt"))

  (define suite
    (test-suite "Can count correct digits"
      (test-eq? "Can determine that 123 is not a square digit"
                (square-digit? 123)
                #f)

      (test-eq? "Can determine that 456456 is a square digit"
                (square-digit? 456456)
                #t)

      (test-eq? "Can determine that 1234512345 is a square digit"
                (square-digit? 1234512345)
                #t)

      (test-eq? "Can determine that 123321 is not a square digit"
                (square-digit? 123321)
                #f)

      (test-eq? "Can sum square digits within basic ranges"
                (sum-square-digits-in-ranges "10-40,12-17")
                66)

      (test-eq? "Can sum square digits for sample ranges"
                (sum-square-digits-in-ranges sample)
                1227775554)

      (test-eq? "Can sum square digits for input.txt part 1"
                (sum-square-digits-in-ranges input)
                18952700150)))
  (run-tests suite))
