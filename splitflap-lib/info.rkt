#lang info

(define collection "splitflap")
(define version "0.9")

(define deps '("gregor-lib"
               "rackunit-lib"
               "txexpr"
               "web-server-lib"
               "base"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "implementation part of \"splitflap\"")
(define license 'BlueOak-1.0.0)
