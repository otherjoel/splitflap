#lang racket/base

;; ~~ RSS Feed Generation ~~~~~~~~~~~~~~~~~~~~~~~~

(require "dust.rkt"
         "xml-generic.rkt"
         "validation.rkt"
         gregor
         (only-in net/url url?)
         racket/contract
         racket/generic
         racket/list
         racket/match
         racket/promise
         racket/runtime-path
         (only-in racket/string non-empty-string?)
         setup/getinfo
         xml)

(provide food?
         (contract-out
          [feed-xslt-stylesheet (parameter/c (or/c non-empty-string? url? #f))]
          [feed-language (parameter/c (or/c iso-639-language-code? #f))]
          [rename make-feed-item feed-item
                  (->* (tag-uri? valid-url-string? string? person? moment? moment? xexpr?)
                       ((or/c enclosure? #f))
                       feed-item?)]
          [rename make-feed feed (-> tag-uri? valid-url-string? string? (listof feed-item?) feed?)]
          [rename make-episode episode
                  (->* (tag-uri? valid-url-string? string? person? moment? moment? xexpr? enclosure?)
                       (#:duration (or/c exact-nonnegative-integer? #f)
                        #:image-url (or/c valid-url-string? #f)
                        #:explicit? any/c
                        #:episode-num (or/c exact-nonnegative-integer? #f)
                        #:season-num (or/c exact-nonnegative-integer? #f)
                        #:type (or/c 'trailer 'full 'bonus #f)
                        #:block? any/c)
                       episode?)]
          [rename make-podcast podcast
                  (->* (tag-uri?
                        valid-url-string?
                        string?
                        (listof episode?)
                        (or/c string? (list/c string? string?))
                        valid-url-string?
                        person?
                        #:explicit? any/c)
                       (#:type (or/c 'serial 'episodic #f)
                        #:block? any/c
                        #:complete? any/c
                        #:new-feed-url (or/c valid-url-string? #f))
                       podcast?)])
         feed-item?
         episode?
         feed?
         podcast?
         include-generator?
         express-xml)

;; ~~ Ancillary elements ~~~~~~~~~~~~~~~~~~~~~~~~~

(define feed-language (make-parameter #f))

;; Build the <generator> tag (caches for each dialect)
(define generator
  (let ([cache (make-hash)])
    (λ (dialect)
      (hash-ref!
       cache
       dialect
       (λ ()
         (define gen-str (format "Racket v~a [~a] + splitflap v~a" (version) (system-type 'gc) (splitflap-version)))
         (case dialect
           [(rss) `(generator ,(string-append gen-str " (https://racket-lang.org)"))]
           [(atom) `(generator [[uri "https://racket-lang.org"] [version ,(version)]] ,gen-str)]))))))

;; Parameter determines whether feed output will include a <generator> tag
;; Useful for keeping unit tests from breaking on different versions
(define include-generator? (make-parameter #t))

;;
;; ~~ Feed Entries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(struct feed-item
  (id url title author published updated content media)
  #:constructor-name feed-item_
  #:methods gen:food
  [(define/generic <-express-xml express-xml)
   (define/contract (express-xml e dialect [url #f] #:as [result-type 'xml-string])
     (->* (any/c rss-dialect?) (any/c #:as xml-type/c) any/c)
     (match-define (feed-item id url title author published updated raw-content media) e)
     (define to-xml? (memq result-type '(xml xml-string xexpr-cdata)))
     (define entry-xpr
       (case dialect
         [(atom)
          `(entry
            (title [[type "text"]] ,title)
            (link [[rel "alternate"] [href ,url]])
            (updated ,(moment->string updated 'atom))
            (published ,(moment->string published 'atom))
            ,@(if/sp media (<-express-xml media 'atom #:as 'xexpr))
            ,(person->xexpr author 'author 'atom)
            (id ,(tag-uri->string id))
            ,(content->safe-element raw-content 'content 'atom to-xml?))]
         [(rss)
          `(item
            (title ,title)
            (link ,url)
            (pubDate ,(moment->string published 'rss))
            ,@(if/sp media (<-express-xml media 'rss #:as 'xexpr))
            ,(person->xexpr author 'author 'rss)
            (guid [[isPermaLink "false"]] ,(tag-uri->string id))
            ,(content->safe-element raw-content 'description 'rss to-xml?))]))
     (case result-type
       [(xexpr xexpr-cdata) entry-xpr]
       [(xml) (xexpr->xml entry-xpr)]
       [(xml-string) (indented-xml-string entry-xpr)]))])

(define (make-feed-item id url title author published updated content [media #f])
  (unless (moment>=? updated published)
    (raise-arguments-error 'feed-item "updated timestamp cannot come before published timestamp"
                           "updated" updated "published" published))
  (feed-item_ id url title author published updated content media))

(define (entry-newer? maybe-newer other)
  (moment>? (feed-item-updated maybe-newer) (feed-item-updated other)))

;;
;; ~~ Feeds ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(struct feed
  (id site-url name entries)
  #:guard (struct-guard/c tag-uri? valid-url-string? string? (listof feed-item?))
  #:constructor-name feed_
  #:methods gen:food
  [(define/generic <-express-xml express-xml)
   (define/contract (express-xml f dialect [feed-url #f] #:as [result-type 'xml-string])
     (->* (any/c rss-dialect?) ((or/c valid-url-string? #f) #:as xml-type/c) (or/c string? document? txexpr?))
     (unless (valid-url-string? feed-url)
       (raise-argument-error 'express-xml "valid URL (required for #<feed>)" feed-url))
     (match-define (feed feed-id site-url feed-name entries) f)
     (define entries-sorted (sort entries entry-newer?))
     (define last-updated (feed-item-updated (car entries-sorted)))
     (define to-xml? (memq result-type '(xml xml-string)))
  
     (define feed-xpr
       (case dialect
         [(atom)
          `(feed [[xmlns "http://www.w3.org/2005/Atom"]
                  [xml:lang ,(symbol->string (or (feed-language) (force system-language)))]]
                 (title [[type "text"]] ,feed-name)
                 (link [[rel "self"] [href ,feed-url]])
                 (link [[rel "alternate"] [href ,site-url]])
                 (updated ,(moment->string last-updated 'atom))
                 (id ,(tag-uri->string feed-id))
                 ,@(if/sp (include-generator?) (generator 'atom))
                 ,@(for/list ([e (in-list entries-sorted)])
                     (<-express-xml e 'atom #:as (if to-xml? 'xexpr-cdata 'xexpr))))]
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
                 (language ,(symbol->string (or (feed-language) (force system-language))))
                 ,@(for/list ([e (in-list entries-sorted)])
                     (<-express-xml e 'rss #:as (if to-xml? 'xexpr-cdata 'xexpr)))))]))
     (case result-type
       [(xexpr xexpr-cdata) feed-xpr]
       [(xml) (xml-document feed-xpr)]
       [(xml-string) (indented-xml-string feed-xpr #:document? #t)]))])

(define (make-feed id site-url name entries)
  (let* ([entry-tags (map feed-item-id entries)]
         [duplicate-tag (check-duplicates (cons id entry-tags) tag=?)])
    (when duplicate-tag
      (raise-arguments-error 'feed
                             "Duplicate tag URI found in feed"
                             "First duplicate encountered" (tag-uri->string duplicate-tag)
                             "Feed tag URI" (tag-uri->string id)
                             "Feed item tag URIs" (map tag-uri->string entry-tags))))
  (feed_ id site-url name entries))

;; ~~ Podcast episodes and feeds ~~~~~~~~~~~~~~~~~

;; Episodes are feed-entries that require an enclosure, and add required and optional tags
;; and flags from Apple’s podcast feed specifications.
(struct episode (id url title author published updated content media
                    duration image-url explicit? episode-n season-n type block?)
  #:constructor-name episode_
  #:methods gen:food
  [(define/generic <-express-xml express-xml)
   (define/contract (express-xml ep dialect [feed-url #f] #:as [result-type 'xml-string])
     (->* (any/c rss-dialect?) (any/c #:as xml-type/c) any/c)
     (define to-xml? (memq result-type '(xml xml-string xexpr-cdata)))
     (match-define
       (episode id url title author published updated content media
                duration image-url explicit? episode-n season-n ep-type block?) ep)
     (define episode-xpr
       `(item
         (title ,title)
         (link ,url)
         (pubDate ,(moment->string updated 'rss))
         ,@(if/sp media (<-express-xml media 'rss #:as 'xexpr))
         ,@(if/sp ep-type `(itunes:episodeType ,(symbol->string ep-type)))
         ,(person->xexpr author 'author 'rss)
         (guid [[isPermaLink "false"]] ,(tag-uri->string id))
         ,(content->safe-element content 'description 'rss to-xml?)
         ,@(if/sp duration `(itunes:duration ,(number->string duration)))
         ,@(if/sp (not (null? explicit?)) `(itunes:explicit ,(if explicit? "true" "false")))
         ,@(if/sp image-url `(itunes:image [[href ,image-url]]))
         ,@(if/sp episode-n `(itunes:episode ,(number->string episode-n)))
         ,@(if/sp season-n `(itunes:season ,(number->string season-n)))
         ,@(if/sp block? '(itunes:block "Yes"))
         ))
     (case result-type
       [(xexpr xexpr-cdata) episode-xpr]
       [(xml) (xexpr->xml episode-xpr)]
       [(xml-string) (indented-xml-string episode-xpr)]))])

(define (make-episode id url title author published updated content media
                      #:duration [duration #f]
                      #:image-url [image-url #f]
                      #:explicit? [explicit? null]
                      #:episode-num [episode-n #f]
                      #:season-num [season-n #f]
                      #:type [type #f]
                      #:block? [block? #f])
  (unless (moment>=? updated published)
    (raise-arguments-error 'feed-item "updated timestamp cannot come before published timestamp"
                           "updated" updated "published" published))
  (episode_ id url title author published updated content media
            duration image-url explicit? episode-n season-n type block?))

;; Podcasts are feeds with more requirements. They will only ever express as RSS 2.0 because that’s
;; what Apple’s podcast directory requires.
(struct podcast (id site-url name entries category image-url owner explicit? type block? complete? new-feed-url)
  #:constructor-name podcast_
  #:methods gen:food
  [(define/generic <-express-xml express-xml)
   (define/contract (express-xml p dialect [feed-url #f] #:as [result-type 'xml-string])
     (->* (any/c rss-dialect?) ((or/c valid-url-string? #f) #:as xml-type/c) (or/c string? document? txexpr?))
     (unless (valid-url-string? feed-url)
       (raise-argument-error 'express-xml "valid URL (required for #<podcast>)" feed-url))
     (match-define (podcast feed-id site-url feed-name episodes cat image-url owner explicit? type block? complete? new-feed-url) p)
     (define episodes-sorted (sort episodes entry-newer?))
     (define category
       (match cat
         [(list cat1 cat2)
          `(itunes:category [[text ,cat1]]
                            (itunes:category [[text ,cat2]]))]
         [(? string? c) `(itunes:category [[text ,c]])]))
     (define last-updated (episode-updated (car episodes-sorted)))
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
              (language ,(symbol->string (or (feed-language) (force system-language))))
              ,(person->xexpr owner 'itunes:owner 'itunes)
              (itunes:image [[href ,image-url]])
              ,category
              (itunes:explicit ,(if explicit? "yes" "no"))
              ,@(if/sp type `(itunes:type ,(symbol->string type)))
              ,@(if/sp block? '(itunes:block "Yes"))
              ,@(if/sp complete? '(itunes:complete "Yes"))
              ,@(if/sp new-feed-url `(itunes:new-feed-url ,new-feed-url))
              ,@(for/list ([e (in-list episodes-sorted)])
                  (<-express-xml e 'rss #:as (if to-xml? 'xexpr-cdata 'xexpr))))))
     (case result-type
       [(xexpr xexpr-cdata) feed-xpr]
       [(xml) (xml-document feed-xpr)]
       [(xml-string) (indented-xml-string feed-xpr #:document? #t)]))])

(define (make-podcast id site-url name episodes category image-url owner
                      #:explicit? explicit?
                      #:type [type #f]
                      #:block? [block? #f]
                      #:complete? [complete? #f]
                      #:new-feed-url [new-feed-url #f])
  (let* ([episode-tags (map episode-id episodes)]
         [duplicate-tag (check-duplicates (cons id episode-tags) tag=?)])
    (when duplicate-tag
      (raise-arguments-error 'podcast
                             "Duplicate tag URI found in podcast feed"
                             "First duplicate encountered" (tag-uri->string duplicate-tag)
                             "Feed tag URI" (tag-uri->string id)
                             "Episode tag URIs" (map tag-uri->string episode-tags))))
  (podcast_ id site-url name episodes category image-url owner explicit? type block? complete? new-feed-url))

;; Version
(define-runtime-path lib-folder "..")

(define (splitflap-version)
  ((get-info/full lib-folder) 'version))
