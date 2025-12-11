#lang racket

(require "day11.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define sample '(
    "aaa: you hhh"
    "you: bbb ccc"
    "bbb: ddd eee"
    "ccc: ddd eee fff"
    "ddd: ggg"
    "eee: out"
    "fff: out"
    "ggg: out"
    "hhh: ccc fff iii"
    "iii: out"))

  (define sample2 '(
    "svr: aaa bbb"
    "aaa: fft"
    "fft: ccc"
    "bbb: tty"
    "tty: ccc"
    "ccc: ddd eee"
    "ddd: hub"
    "hub: fff"
    "eee: dac"
    "dac: fff"
    "fff: ggg hhh"
    "ggg: out"
    "hhh: out"))
  
  (define input (file->lines "input.txt"))

  (define suite
    (test-suite "Reactor: Device attachments test suite"
                
      (test-case "Part 1 - Single direct connection you -> out"
        (check-equal? (find-total-paths '("you: out" "out:")) 1))

      (test-case "Part 1 - No path you -> out"
        (check-equal? (find-total-paths '("you: aaa" "aaa: bbb" "bbb:" "out:")) 0))

      (test-case "Part 1 - Multiple simple parallel paths"
        (check-equal? (find-total-paths '("you: a b" "a: out" "b: out" "out:")) 2))

      (test-case "Part 1 - Single branching chain with one dead end"
        ;; you → a → out
        ;; you → b → c (dead end)
        (check-equal?
         (find-total-paths '("you: a b"
                             "a: out"
                             "b: c"
                             "c:"
                             "out:"))
         1))

      (test-case "Part 1 - Three-step linear path"
        (check-equal?
         (find-total-paths '("you: a"
                             "a: b"
                             "b: out"
                             "out:"))
         1))
                
      (test-case "Can solve sample case for Part 1"
        (check-equal? (find-total-paths sample) 5))

      (test-case "Can find the correct result from input.txt for Part 1"
        (check-equal? (find-total-paths input) 746))

      (test-case "Part 2 - no path to out"
        (check-equal?
         (find-total-paths-2 '("svr: a" "a: b" "b:" "dac:" "fft:" "out:")) 0))

      (test-case "Part 2 - paths exist but missing dac or fft"
        (check-equal?
         (find-total-paths-2
          '("svr: a b"
            "a: out"
            "b: out"
            "dac:"
            "fft:"
            "out:"))
         0))

      (test-case "Part 2 - exactly one satisfying path"
        (check-equal?
         (find-total-paths-2
          '("svr: a"
            "a: fft"
            "fft: dac"
            "dac: out"
            "out:"))
         1))

      (test-case "Part 2 - two valid paths"
        (check-equal?
         (find-total-paths-2
          '("svr: x y"
            "x: fft"
            "y: fft"
            "fft: dac"
            "dac: out"
            "out:"))
         2))

      (test-case "Can solve sample case for Part 2"
        (check-equal? (find-total-paths-2 sample2) 2))

      (test-case "Can find the correct result from input.txt for Part 2"
        (check-equal? (find-total-paths-2 input) 370500293582760))))
  (run-tests suite))
