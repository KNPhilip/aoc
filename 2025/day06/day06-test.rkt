#lang racket

(require "day06.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define input (file->string "input.txt"))
  (define sample "123 328  51 64\n 45 64  387 23\n  6 98  215 314\n*   +   *   +  ")
  
  (define suite
    (test-suite "Cephalopod mathematics"
      (test-eq? "Simple addition works"
                (compute-total "3\n3\n+")
                6)

      (test-eq? "Simple multiplication works"
                (compute-total "3\n3\n*")
                9)
                
      (test-eq? "Can solve sample case for Part 1"
                (compute-total sample)
                4277556)

      (test-eq? "Can find the correct result from input.txt for Part 1"
                (compute-total input)
                4412382293768)

      (test-eq? "Can solve sample case for Part 2"
                (compute-total-2 sample)
                3263827)

      (test-eq? "Can find the correct result from input.txt for Part 2"
                (compute-total-2 input)
                7858808482092)))
  (run-tests suite))
