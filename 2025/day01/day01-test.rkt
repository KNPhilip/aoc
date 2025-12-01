#lang racket

(require "day01.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define input-list
    (port->lines (open-input-file "input.txt")))
  
  (define suite
    (test-suite "Can count correct safe moves"
      (test-eq? "Safe moves never lands on 0"
                (count-safe-moves '("L40" "L20" "R40" "L50" "R99") 0)
                0)

      (test-eq? "Safe moves never lands on 1"
                (count-safe-moves '("R50" "L25" "R42" "L30" "R15") 1)
                0)

      (test-eq? "Safe move lands on 0 twice"
                (count-safe-moves '("L50" "L20" "R20" "L50" "R99") 0)
                2)

      (test-eq? "Safe move lands on 5 thrice"
                (count-safe-moves '("R5" "L50" "R20" "L20" "R1" "L1") 5)
                3)

      (test-eq? "Safe moves from input.txt outputs correct result for part 1"
                (count-safe-moves input-list 0)
                962)
      
      (test-eq? "Basic new safe moves can be calculated"
                (count-safe-moves-part2 '("R60" "R10" "L80" "L400"))
                6)

      (test-eq? "Complex new safe moves can be calculated"
                (count-safe-moves-part2 '("R650" "R30" "L280" "L4" "R52" "L253" "L20" "L67" "R900" "R453"))
                27)

      (test-eq? "New safe moves calculates correct input.txt result for part 2"
                (count-safe-moves-part2 input-list)
                5782)))
  (run-tests suite))
