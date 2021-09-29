#lang racket/base

(require "constructs.rkt"
         gregor
         txexpr
         racket/contract
         racket/match
         racket/port
         xml)

(define (as-cdata v)
  (cdata #f #f (format "<![CDATA[~a]]>"
                       (cond [(txexpr? v) (xexpr->string v)]
                             [else v]))))

(struct feed-entry
  (id url title author published updated content)
  #:guard (struct-guard/c
           tag-uri?
           valid-url-string?
           string?
           person?
           moment?
           moment?
           xexpr?))

(define (entry-newer? maybe-newer other)
  (moment>? (feed-entry-updated maybe-newer) (feed-entry-updated other)))

(define/contract (entry-output e dialect #:result-type [result-type 'xml-string])
  (->* (feed-entry? rss-dialect?) (#:result-type (or/c 'xml-string 'xml 'xexpr 'xexpr-cdata)) any/c)
  (match-define (feed-entry id url title author published updated content) e)
  (define xml? (memq result-type '(xml xml-string xexpr-cdata)))
  (define entry-xpr
  (case dialect
    [(atom)
     `(entry
       (title ,title)
       (id ,(tag-uri->string id))
       (link [[rel "alternate"] [href ,url]])
       ,(person->xexpr author 'author 'atom)
       (updated ,(moment->string updated 'atom))
       (published ,(moment->string published 'atom))  
       (content ,(if xml? (as-cdata content) (cdata-string (as-cdata content)))))]
    [(rss)
     `(item
       (title ,title)
       ,(person->xexpr author 'author 'rss)
       (link ,(tag-uri->string id))
       (guid [[isPermaLink "false"]] ,(tag-uri->string id))
       (pubDate ,(moment->string updated 'rss))
       (description ,(if xml? (as-cdata content) (cdata-string (as-cdata content)))))]))
  (case result-type
    [(xexpr xexpr-cdata) entry-xpr]
    [(xml) (xexpr->xml entry-xpr)]
    [(xml-string) (feed-xml-string (xexpr->xml entry-xpr))]))

(struct feed
  (id site-url name entries)
  #:guard (struct-guard/c tag-uri? valid-url-string? xexpr? (listof feed-entry?)))

(define (feed-xml-string x)
  (define display-proc (if (document? x) display-xml display-xml/content))
  (with-output-to-string
    (λ ()
      (parameterize ([empty-tag-shorthand (cons 'atom:link html-empty-tags)])
        (display-proc x #:indentation 'peek)))))

(define (xml-document xpr)
  (document
   (prolog (list (p-i 'racket 'racket 'xml "version=\"1.0\" encoding=\"UTF-8\"")) #f '())
   (xexpr->xml xpr)
   '()))

(define/contract (feed-output f feed-url dialect #:result-type [result-type 'xml-string])
  (->* (feed? valid-url-string? rss-dialect?) (#:result-type (or/c 'xml-string 'xml 'xexpr)) (or/c string? document? txexpr?))
  (define entries-sorted (sort (feed-entries f) entry-newer?))
  (match-define (feed feed-id site-url feed-name _) f)
  (define gen (format "Racket v~a [~a] (https://racket-lang.org)" (version) (system-type 'gc)))
  (define last-updated (feed-entry-updated (car entries-sorted)))
  (define xml? (memq result-type '(xml xml-string)))
  
  (define feed-xpr
    (case dialect
      [(atom)
       `(feed [[xmlns "http://www.w3.org/2005/Atom"]]
              (title ,feed-name)
              (generator ,gen)
              (id ,(tag-uri->string feed-id))
              (link [[rel "self"] [href ,feed-url]])
              (link [[rel "alternate"] [href ,site-url]])
              (updated ,(moment->string last-updated 'atom))
              ,@(for/list ([e (in-list entries-sorted)])
                  (entry-output e 'atom #:result-type (if xml? 'xexpr-cdata 'xexpr))))]
      [(rss)
       `(rss [[version "2.0"] [xmlns:atom "http://www.w3.org/2005/Atom"]]
             (channel
              (title ,feed-name)
              (generator ,gen)
              (description ,feed-name)
              (atom:link [[rel "self"] [href ,feed-url] [type "application/rss+xml"]])
              (link ,site-url)
              (pubDate ,(moment->string last-updated 'rss))
              (lastBuildDate ,(moment->string last-updated 'rss))
              ,@(for/list ([e (in-list entries-sorted)])
                  (entry-output e 'atom #:result-type (if xml? 'xexpr-cdata 'xexpr)))))]))
  (case result-type
    [(xexpr) feed-xpr]
    [(xml) (xml-document feed-xpr)]
    [(xml-string) (feed-xml-string (xml-document feed-xpr))]))
