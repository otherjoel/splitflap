#lang info

(define collection "splitflap")
(define version "0.9")

(define deps '("gregor-lib"
               "rackunit-lib"
               "txexpr"
               "base"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))
(define compile-omit-paths '("private/build.rkt"))
(define pkg-desc "implementation part of \"splitflap\"")
(define license 'BlueOak-1.0.0)
