#lang racket

(require "day05.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define input (string-join (file->lines "input.txt") "\n"))
  (define sample
    (string-join
     '("3-5"
       "10-14"
       "16-20"
       "12-18"
       ""
       "1"
       "5"
       "8"
       "11"
       "17"
       "32")
     "\n"))
  
  (define suite
    (test-suite "Analysis of the Ingredients Management System"
      (test-eq? "Counts simple ids within range"
                (count-fresh-ids "2-6\n\n3\n4\n10\n5")
                3)
                
      (test-eq? "Can solve sample case for Part 1"
                (count-fresh-ids sample)
                3)

      (test-eq? "Can find the correct result from input.txt for Part 1"
                (count-fresh-ids input)
                664)

      (test-eq? "Counts simple id ranges"
                (count-fresh-range-ids "2-6\n10-20\n77-79\n\n3\n4\n10\n5")
                19)
      
      (test-eq? "Can solve sample case for Part 2"
                (count-fresh-range-ids sample)
                14)

      (test-eq? "Can find the correct result from input.txt for Part 2"
                (count-fresh-range-ids input)
                350780324308385)))
  (run-tests suite))
