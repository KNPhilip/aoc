#lang racket

(require "day07.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define input (file->lines "input.txt"))
  (define sample
    (string-join
      (list
        ".......S......."
        "..............."
        ".......^......."
        "..............."
        "......^.^......"
        "..............."
        ".....^.^.^....."
        "..............."
        "....^.^...^...."
        "..............."
        "...^.^...^.^..."
        "..............."
        "..^...^.....^.."
        "..............."
        ".^.^.^.^.^...^."
        "...............") "\n"))
  
  (define suite
    (test-suite "Manifold splits correctly"
      (test-eq? "Can solve sample case for Part 1"
                (solve-laboratories sample)
                21)

      (test-eq? "Can find the correct result from input.txt for Part 1"
                (solve-laboratories input)
                1633)))
  (run-tests suite))
