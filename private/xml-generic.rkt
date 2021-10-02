#lang racket/base

(require racket/contract racket/generic)

(provide gen:xml-source
         express-xml
         xml-type/c)

;; Generic interface for structs that support converting to XML or xexprs

(define-generics xml-source
  (express-xml xml-source dialect url #:as [result-type]))

;; Acceptable values for the #:as argument of express-xml
(define xml-type/c (or/c 'xml-string 'xml 'xexpr 'xexpr-cdata))