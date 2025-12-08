#lang racket

(require "day08.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define (pts->lines pts)
    (for/list ([p pts]) (format "~a,~a,~a" (first p) (second p) (third p))))

  (define sample
    '("162,817,812"
      "57,618,57"
      "906,360,560"
      "592,479,940"
      "352,342,300"
      "466,668,158"
      "542,29,236"
      "431,825,988"
      "739,650,466"
      "52,470,668"
      "216,146,977"
      "819,987,18"
      "117,168,530"
      "805,96,715"
      "346,949,466"
      "970,615,88"
      "941,993,340"
      "862,61,35"
      "984,92,344"
      "425,690,689"))

  (define input (file->lines "input.txt"))

  (define suite
    (test-suite "Playground - expanded"
      (test-case "Part 1: Sample k=10 -> 40"
        (check-equal? (solve-playground sample 10) 40))

      (test-case "Part 1: First 10 points, k=10 -> 10"
        (check-equal? (solve-playground (take sample 10) 10) 10))

      (test-case "Part 1: Sample with 1000 connections (fully connected) -> 20"
        (check-equal? (solve-playground sample 1000) 20))

      (test-case "Part 1: input.txt"
        (check-equal? (solve-playground input 1000) 79560))

      (test-case "Part 1: 1 point"
        (define l (pts->lines '((0 0 0))))
        (check-equal? (solve-playground l 1000) 1)
        (check-equal? (solve-playground l 0) 1))

      (test-case "Part 1: 3 points line (k=1 -> 2, k=2 -> 3)"
        (define l (pts->lines '((0 0 0) (3 0 0) (10 0 0))))
        (check-equal? (solve-playground l 1) 2)
        (check-equal? (solve-playground l 2) 3))

      (test-case "Part 1: two near pairs + single, k=2 -> 4"
        (define l (pts->lines '((0 0 0) (1 0 0) (10 0 0) (11 0 0) (100 0 0))))
        (check-equal? (solve-playground l 2) 4))

      (test-case "Part 1: All duplicates (4 same points) k=1 -> 2"
        (define l (pts->lines '((7 7 7) (7 7 7) (7 7 7) (7 7 7))))
        (check-equal? (solve-playground l 1) 2))))

  (run-tests suite))
