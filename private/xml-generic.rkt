#lang racket/base

(require racket/contract racket/generic)

(provide gen:food
         food?
         express-xml
         xml-type/c)

;; Generic interface for structs that support converting to XML or xexprs in RSS or Atom

(define-generics food
  (express-xml food dialect url #:as [result-type]))

;; Acceptable values for the #:as argument of express-xml
(define xml-type/c (or/c 'xml-string 'xml 'xexpr 'xexpr-cdata))