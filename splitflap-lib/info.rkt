#lang info

(define collection "splitflap")
(define version "1.2")

(define deps '(["base" #:version "8.1"]
               "gregor-lib"))

(define compile-omit-paths '("private/build.rkt"))
(define pkg-desc "implementation part of \"splitflap\"")
(define license 'BlueOak-1.0.0)
