#lang info

(define collection "feed")
(define scribblings '(("feed.scrbl")))

(define deps '("base"))
(define build-deps '("scribble-lib"
                     "feed-lib"))

(define update-implies '("feed-lib"))

(define pkg-desc "documentation part of \"feed\"")
(define license 'BlueOak-1.0.0)
