#lang info

(define collection "splitflap")
(define scribblings '(("splitflap.scrbl" (multi-page))))

(define deps '("scribble-lib"
               "base"))
(define build-deps '("net-doc"
                     "txexpr"
                     "gregor-doc"
                     "gregor-lib"
                     "racket-doc"
                     "scribble-lib"
                     "splitflap-lib"))

(define update-implies '("splitflap-lib"))

(define pkg-desc "documentation part of \"splitflap\"")
(define license 'BlueOak-1.0.0)
