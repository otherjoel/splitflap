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

;; ~~ Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define feed-locale
  (make-parameter
   (match
       (case (system-type 'os)
         [(unix macosx) (bytes->string/utf-8 (system-language+country))]
         [(windows)
          ((dynamic-require 'file/resource 'get-resource)
           "HKEY_CURRENT_USER" "Control Panel\\International\\LocaleName")])
     [(? string? loc) (string->symbol (substring loc 0 2))]
     [_ #f])
   (λ (v)
     (unless (or (iso-639-language-code? v) (not v))
       (raise-argument-error 'feed-locale "iso-639-language-code? (or #false)" v))
     v)))

;; Build the <generator> tag (caches for each dialect)
(define generator
  (let ([cache (make-hash)])
    (λ (dialect)
      (hash-ref!
       cache
       dialect
       (λ ()
         (define gen-str (format "Racket v~a [~a]" (version) (system-type 'gc)))
         (case dialect
           [(rss) `(generator ,(string-append gen-str " (https://racket-lang.org)"))]
           [(atom) `(generator [[uri "https://racket-lang.org"] [version ,(version)]] ,gen-str)]))))))

;; Parameter determines whether feed output will include a <generator> tag
;; Useful for keeping unit tests from breaking on different versions
(define include-generator? (make-parameter #t))

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
            (link [[rel "alternate"] [href ,url]])
            (updated ,(moment->string updated 'atom))
            (published ,(moment->string published 'atom))
            ,@(if/sp media (<-express-xml media 'atom #f))
            ,(person->xexpr author 'author 'atom)
            (id ,(tag-uri->string id))
            (content ,(if to-xml? (as-cdata content) (cdata-string (as-cdata content)))))]
         [(rss)
          `(item
            (title ,title)
            (link ,url)
            (pubDate ,(moment->string published 'rss))
            ,@(if/sp media (<-express-xml media 'rss #f))
            ,(person->xexpr author 'author 'rss)
            (guid [[isPermaLink "false"]] ,(tag-uri->string id))
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
                 (link [[rel "self"] [href ,feed-url]])
                 (link [[rel "alternate"] [href ,site-url]])
                 (updated ,(moment->string last-updated 'atom))
                 (id ,(tag-uri->string feed-id))
                 ,@(if/sp (include-generator?) (generator 'atom))
                 ,@(for/list ([e (in-list entries-sorted)])
                     (<-express-xml e 'atom #f #:as (if to-xml? 'xexpr-cdata 'xexpr))))]
         [(rss)
          `(rss [[version "2.0"] [xmlns:atom "http://www.w3.org/2005/Atom"]]
                (channel
                 (title ,feed-name)
                 (atom:link [[rel "self"] [href ,feed-url] [type "application/rss+xml"]])
                 (link ,site-url)
                 (pubDate ,(moment->string last-updated 'rss))
                 (lastBuildDate ,(moment->string last-updated 'rss))
                 ,@(if/sp (include-generator?) (generator 'rss))
                 (description ,feed-name)
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
                duration image-url explicit? episode-n season-n ep-type block?) ep)
     (define episode-xpr
       `(item
         (title ,title)
         (link ,url)
         (pubDate ,(moment->string updated 'rss))
         ,@(if/sp media (<-express-xml media 'rss #f))
         ,@(if/sp ep-type `(itunes:episodeType ,(symbol->string ep-type)))
         ,(person->xexpr author 'author 'rss)
         (guid [[isPermaLink "false"]] ,(tag-uri->string id))
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

(struct podcast feed (category image-url owner explicit? type block? complete? new-feed-url)
  #:constructor-name podcast_
  #:methods gen:food
  [(define/generic <-express-xml express-xml)
   (define/contract (express-xml p dialect feed-url #:as [result-type 'xml-string])
     (->* (any/c rss-dialect? valid-url-string?) (#:as xml-type/c) (or/c string? document? txexpr?))

     (match-define (podcast feed-id site-url feed-name episodes cat image-url owner explicit? type block? complete? new-feed-url) p)
     (define episodes-sorted (sort episodes entry-newer?))
     (define category
       (match cat
         [(list cat1 cat2)
          `(itunes:category [[text ,cat1]]
                            (itunes:category [[text ,cat2]]))]
         [(? string? c) `(itunes:category [[text ,c]])]))
     (define last-updated (feed-entry-updated (car episodes-sorted)))
     (define to-xml? (memq result-type '(xml xml-string)))

     (define feed-xpr
       `(rss [[version "2.0"]
              [xmlns:atom "http://www.w3.org/2005/Atom"]
              [xmlns:itunes "http://www.itunes.com/dtds/podcast-1.0.dtd"]]
             (channel
              (title ,feed-name)
              (atom:link [[rel "self"] [href ,feed-url] [type "application/rss+xml"]])
              (link ,site-url)
              (pubDate ,(moment->string last-updated 'rss))
              (lastBuildDate ,(moment->string last-updated 'rss))
              ,@(if/sp (include-generator?) (generator 'rss))
              (description ,feed-name)
              (language "en-US")
              ,(person->xexpr owner 'itunes:owner 'atom #:elem-prefix 'itunes:)
              (itunes:image [[href ,image-url]])
              ,category
              (itunes:explicit ,(if explicit? "yes" "no"))
              ,@(if/sp type `(itunes:type ,(symbol->string type)))
              ,@(if/sp block? '(itunes:block "Yes"))
              ,@(if/sp complete? '(itunes:complete "Yes"))
              ,@(if/sp new-feed-url `(itunes:new-feed-url ,new-feed-url))
              ,@(for/list ([e (in-list episodes-sorted)])
                  (<-express-xml e 'rss #f #:as (if to-xml? 'xexpr-cdata 'xexpr))))))
     (case result-type
       [(xexpr xexpr-cdata) feed-xpr]
       [(xml) (xml-document feed-xpr)]
       [(xml-string) (indented-xml-string feed-xpr #:document? #t)]))])

(define/contract (make-podcast id site-url name entries category image-url owner explicit?
                               #:type [type #f]
                               #:block? [block? #f]
                               #:complete? [complete? #f]
                               #:new-feed-url [new-feed-url #f])
  (->* (tag-uri?
        valid-url-string?
        xexpr?
        (listof feed-entry?)
        (or/c string? (list/c string? string?))
        valid-url-string?
        person?
        boolean?)
       (#:type (or/c 'serial 'episodic #f)
        #:block? boolean?
        #:complete? boolean?
        #:new-feed-url (or/c valid-url-string? #f))
       podcast?)
  (podcast_ id site-url name entries category image-url owner explicit? type block? complete? new-feed-url))
