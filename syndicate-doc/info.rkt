#lang info

(define collection "feed")
(define scribblings '(("syndicate.scrbl")))

(define deps '("scribble-lib"
               "base"))
(define build-deps '("gregor-doc"
                     "gregor-lib"
                     "racket-doc"
                     "scribble-lib"
                     "syndicate-lib"))

(define update-implies '("syndicate-lib"))

(define pkg-desc "documentation part of \"syndicate\"")
(define license 'BlueOak-1.0.0)
