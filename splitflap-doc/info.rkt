#lang info

(define collection "splitflap")
(define scribblings '(("splitflap.scrbl" (multi-page))))

(define deps '("scribble-lib"
               "base"))
(define build-deps '("at-exp-lib"
                     "net-doc"
                     "txexpr"
                     "gregor-doc"
                     "gregor-lib"
                     "racket-doc"
                     "scribble-lib"
                     "splitflap-lib"))

(define update-implies '("splitflap-lib"))
(define compile-omit-paths '("js" "styles"))
(define pkg-desc "documentation part of \"splitflap\"")
(define license 'LicenseRef-CreatorCxn-1.0)
