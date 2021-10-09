#lang racket/base

;; ~~ RSS Feed Generation ~~~~~~~~~~~~~~~~~~~~~~~~

(require "constructs.rkt"
         "private/dust.rkt"
         "private/xml-generic.rkt"
         gregor
         txexpr
         racket/contract
         racket/generic
         racket/match
         xml)

(provide food?
         (rename-out [make-feed-entry feed-entry]
                     [make-episode episode])
         (all-from-out "constructs.rkt")
         feed-entry?
         episode?
         feed
         feed?
         podcast?
         include-generator?
         express-xml)

(struct feed-entry
  (id url title author published updated content media)
  #:constructor-name feed-entry_
  #:methods gen:food
  [(define/generic <-express-xml express-xml)
   (define/contract (express-xml e dialect url #:as [result-type 'xml-string])
     (->* (any/c rss-dialect? any/c) (#:as xml-type/c) any/c)
     
     (match-define (feed-entry id url title author published updated content media) e)
     (define to-xml? (memq result-type '(xml xml-string xexpr-cdata)))
     (define entry-xpr
       (case dialect
         [(atom)
          `(entry
            (title [[type "text"]] ,title)
            (id ,(tag-uri->string id))
            (link [[rel "alternate"] [href ,url]])
            ,@(if/sp media (<-express-xml media 'atom #f))
            ,(person->xexpr author 'author 'atom)
            (updated ,(moment->string updated 'atom))
            (published ,(moment->string published 'atom))  
            (content ,(if to-xml? (as-cdata content) (cdata-string (as-cdata content)))))]
         [(rss)
          `(item
            (title ,title)
            ,(person->xexpr author 'author 'rss)
            (link ,url)
            (guid [[isPermaLink "false"]] ,(tag-uri->string id))
            ,@(if/sp media (<-express-xml media 'rss #f))
            (pubDate ,(moment->string updated 'rss))
            (description ,(if to-xml? (as-cdata content) (cdata-string (as-cdata content)))))]))
     (case result-type
       [(xexpr xexpr-cdata) entry-xpr]
       [(xml) (xexpr->xml entry-xpr)]
       [(xml-string) (indented-xml-string entry-xpr)]))])

(define/contract (make-feed-entry id url title author published updated content [media #f])
  (->* (tag-uri? valid-url-string? string? person? moment? moment? xexpr?)
       ((or/c enclosure? #f))
       feed-entry?)
  (feed-entry_ id url title author published updated content media))

(define (entry-newer? maybe-newer other)
  (moment>? (feed-entry-updated maybe-newer) (feed-entry-updated other)))

(struct feed
  (id site-url name entries)
  #:guard (struct-guard/c tag-uri? valid-url-string? xexpr? (listof feed-entry?))

  #:methods gen:food
  [(define/generic <-express-xml express-xml)
   (define/contract (express-xml f dialect feed-url #:as [result-type 'xml-string])
     (->* (any/c rss-dialect? valid-url-string?) (#:as xml-type/c) (or/c string? document? txexpr?))

     (match-define (feed feed-id site-url feed-name entries) f)
     (define entries-sorted (sort entries entry-newer?))
     (define last-updated (feed-entry-updated (car entries-sorted)))
     (define to-xml? (memq result-type '(xml xml-string)))
  
     (define feed-xpr
       (case dialect
         [(atom)
          `(feed [[xmlns "http://www.w3.org/2005/Atom"]]
                 (title ,feed-name)
                 ,@(if/sp (include-generator?) (list (generator 'atom)))
                 (id ,(tag-uri->string feed-id))
                 (link [[rel "self"] [href ,feed-url]])
                 (link [[rel "alternate"] [href ,site-url]])
                 (updated ,(moment->string last-updated 'atom))
                 ,@(for/list ([e (in-list entries-sorted)])
                     (<-express-xml e 'atom #f #:as (if to-xml? 'xexpr-cdata 'xexpr))))]
         [(rss)
          `(rss [[version "2.0"] [xmlns:atom "http://www.w3.org/2005/Atom"]]
                (channel
                 (title ,feed-name)
                 ,@(if/sp (include-generator?) (list (generator 'rss)))
                 (description ,feed-name)
                 (atom:link [[rel "self"] [href ,feed-url] [type "application/rss+xml"]])
                 (link ,site-url)
                 (pubDate ,(moment->string last-updated 'rss))
                 (lastBuildDate ,(moment->string last-updated 'rss))
                 ,@(for/list ([e (in-list entries-sorted)])
                     (<-express-xml e 'rss #f #:as (if to-xml? 'xexpr-cdata 'xexpr)))))]))
     (case result-type
       [(xexpr xexpr-cdata) feed-xpr]
       [(xml) (xml-document feed-xpr)]
       [(xml-string) (indented-xml-string feed-xpr #:document? #t)]))])

;; ~~ Podcast feeds and episodes ~~~~~~~~~~~~~~~~~

(struct episode feed-entry (duration image-url explicit? episode-n season-n type block?)
  #:constructor-name episode_
  #:methods gen:food
  [(define/generic <-express-xml express-xml)
   (define/contract (express-xml ep dialect feed-url #:as [result-type 'xml-string])
     (->* (any/c rss-dialect? any/c) (#:as xml-type/c) any/c)
     (define to-xml? (memq result-type '(xml xml-string xexpr-cdata)))
     (match-define
       (episode id url title author published updated content media
                duration image-url explicit? episode-n season-n type block?) ep)
     (define episode-xpr
       `(item
         (title ,title)
         ,(person->xexpr author 'author 'rss)
         ,@(if/sp type `(itunes:episodeType ,(symbol->string type)))
         (link ,url)
         (guid [[isPermaLink "false"]] ,(tag-uri->string id))
         ,@(if/sp media (<-express-xml media 'rss #f))
         (pubDate ,(moment->string updated 'rss))
         (description ,(if to-xml? (as-cdata content) (cdata-string (as-cdata content))))
         ,@(if/sp duration `(itunes:duration ,(number->string duration)))
         ,@(if/sp (boolean? explicit?) `(itunes:explicit ,(if explicit? "true" "false")))
         ,@(if/sp image-url `(itunes:image [[href ,image-url]]))
         ,@(if/sp episode-n `(itunes:episode ,(number->string episode-n)))
         ,@(if/sp season-n `(itunes:season ,(number->string season-n)))
         ,@(if/sp block? '(itunes:block "Yes"))
         ))
       (case result-type
         [(xexpr xexpr-cdata) episode-xpr]
         [(xml) (xexpr->xml episode-xpr)]
         [(xml-string) (indented-xml-string episode-xpr)]))])

(define/contract (make-episode id url title author published updated content media
                               #:duration [duration #f]
                               #:image-url [image-url #f]
                               #:explicit? [explicit? '_]
                               #:episode-num [episode-n #f]
                               #:season-num [season-n #f]
                               #:type [type #f]
                               #:block? [block? #f])
  (->* (tag-uri? valid-url-string? string? person? moment? moment? xexpr? enclosure?)
       (#:duration (or/c exact-nonnegative-integer? #f)
        #:image-url (or/c valid-url-string? #f)
        #:explicit? (or/c boolean? '_)
        #:episode-num (or/c exact-nonnegative-integer? #f)
        #:season-num (or/c exact-nonnegative-integer? #f)
        #:type (or/c 'trailer 'full 'bonus #f)
        #:block? boolean?)
       episode?)
  (episode_ id url title author published updated content media
            duration image-url explicit? episode-n season-n type block?))

(struct podcast feed (category image-url explicit? type block? complete? new-feed-url)
  #:constructor-name podcast_)

(define/contract (make-podcast id site-url name entries category image-url explicit?
                               #:type [type #f]
                               #:block? [block? #f]
                               #:complete? [complete? #f]
                               #:new-feed-url [new-feed-url #f])
  (->* (tag-uri? valid-url-string? xexpr? (listof feed-entry?) (or/c string? (listof string?)) valid-url-string? boolean?)
       (#:type (or/c 'serial 'episodic #f)
        #:block? boolean?
        #:complete? boolean?
        #:new-feed-url (or/c valid-url-string? #f))
       podcast?)
  (podcast_ id site-url name entries category image-url explicit? type block? complete? new-feed-url))