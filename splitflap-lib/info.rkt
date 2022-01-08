#lang info

(define collection "splitflap")
(define version "1.0")

(define deps '("gregor-lib"
               "rackunit-lib"
               "txexpr"
               "base"))
(define build-deps '("rackunit-lib"))
(define compile-omit-paths '("private/build.rkt"))
(define pkg-desc "implementation part of \"splitflap\"")
(define license 'BlueOak-1.0.0)
