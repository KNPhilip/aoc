#lang racket/base

(require "core.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite "Testing of addition"
      (test-eq? "2 + 2"
                (plus 2 2)
                4)
    ))

  (run-tests suite))
