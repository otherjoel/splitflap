#lang racket/base

(require racket/include
         racket/match)

(provide mime-types-table
         path/string->mime-type)

(define mime-types-by-ext
  (include (lib "splitflap/private/mime-types.rktd")))

(define (mime-types-table) mime-types-by-ext)

;; Return a MIME type for a given file extension.
;; MIME types are not required; if unknown, the type should not be specified.
(define (path/string->mime-type p)
  (match (if (path? p) (path->string p) p)
    [(regexp #rx".*\\.([^\\.]*$)" (list _ ext))
     (hash-ref mime-types-by-ext
               (string->symbol (string-downcase ext))
               #f)]
    [_ #f]))
