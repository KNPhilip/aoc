#lang racket

(require "day12.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define input (file->lines "input.txt"))

  (define suite
    (test-suite "Christmas Tree Farm Test Suite"
      (test-case "Merry Christmas"
        (check-equal? (solve input) 408))))
  (run-tests suite))
