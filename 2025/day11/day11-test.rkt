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
  
  (define input (file->lines "input.txt"))

  (define suite
    (test-suite "Reactor: Device attachments test suite"
                
      (test-case "Single direct connection you -> out"
        (check-equal? (find-total-paths '("you: out" "out:")) 1))

      (test-case "No path you -> out"
        (check-equal? (find-total-paths '("you: aaa" "aaa: bbb" "bbb:" "out:")) 0))

      (test-case "Multiple simple parallel paths"
        (check-equal? (find-total-paths '("you: a b" "a: out" "b: out" "out:")) 2))

      (test-case "Single branching chain with one dead end"
        ;; you → a → out
        ;; you → b → c (dead end)
        (check-equal?
         (find-total-paths '("you: a b"
                             "a: out"
                             "b: c"
                             "c:"
                             "out:"))
         1))

      (test-case "Three-step linear path"
        (check-equal?
         (find-total-paths '("you: a"
                             "a: b"
                             "b: out"
                             "out:"))
         1))
                
      (test-case "Can solve sample case for Part 1"
        (check-equal? (find-total-paths sample) 5))

      (test-case "Can find the correct result from input.txt for Part 1"
        (check-equal? (find-total-paths input) 746))))
  (run-tests suite))
