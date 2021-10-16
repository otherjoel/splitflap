#lang info

(define collection "feed")
(define version "0.9")

(define deps '("gregor-lib"
               "rackunit-lib"
               "txexpr"
               "web-server-lib"
               "base"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "implementation part of \"feed\"")
(define license 'BlueOak-1.0.0)