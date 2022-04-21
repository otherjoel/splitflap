#lang info

(define collection "splitflap")

(define build-deps '("gregor-lib"
                     "rackunit-lib"
                     "splitflap-lib"))
(define deps '("base"))
(define update-implies '("splitflap-lib"))

(define pkg-desc "tests part of \"splitflap\"")
(define license 'BlueOak-1.0.0)