#lang racket/base

(require "constructs.rkt"
         gregor
         pollen/template
         txexpr
         racket/contract)

(define (as-cdata string)
  (format "<![CDATA[~a]]>" string))

(struct feed-entry (id uri title author published updated content)
  #:guard (struct-guard/c tag-specific-string?
                          valid-url-string?
                          string?
                          person?
                          moment?
                          moment?
                          xexpr?))

(define/contract (entry->xexpr e feed-uri dialect)
  (-> feed-entry? tag-uri? rss-dialect? txexpr?)
  (case dialect
    [(rss)
     `(item
       (title ,(feed-entry-title e))
       ,(person->xexpr (feed-entry-author e) 'author 'rss)
       (link ,(feed-entry-uri e))
       (guid ,(string-append (tag-uri->string feed-uri) "." (feed-entry-id e)))
       (pubDate ,(moment->string (feed-entry-published e) 'rss))
       (description ,(as-cdata (feed-entry-content e))))]
    [(atom)
     `(entry
       (title ,(feed-entry-title e))
       (id ,(string-append (tag-uri->string feed-uri) "." (feed-entry-id e)))
       (link [[rel "alternate"] [href ,(feed-entry-uri e)]])
       ,(person->xexpr (feed-entry-author e) 'author 'atom)
       (updated ,(moment->string (feed-entry-updated e) 'atom))
       (published ,(moment->string (feed-entry-published e) 'atom))       
       (content ,(as-cdata (feed-entry-content e))))]))