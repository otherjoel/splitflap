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
         feed-entry?
         episode?
         feed
         feed?
         podcast?
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

(module+ test
  (require rackunit)
  (define site-id (mint-tag-uri "example.com" "2007" "blog"))
  (define e1
    (parameterize ([current-timezone 0])
      (make-feed-entry (append-specific site-id "one")
                       "https://example.com/blog/one.html"
                       "Kate's First Post"
                       (person "Kate Poster" "kate@example.com")
                       (infer-moment "2007-03-17")
                       (infer-moment "2007-03-17")
                       `(div (p "Welcome to my blog.") (ul (li "& ' < > © ℗ ™")))
                       (enclosure "gopher://umn.edu/greeting.m4a" "audio/x-m4a" 1234))))

  (define expect-atom #<<ATOM
<entry>
  <title type="text">Kate&apos;s First Post</title>
  <id>tag:example.com,2007:blog.one</id>
  <link rel="alternate" href="https://example.com/blog/one.html" />
  <link rel="enclosure" type="audio/x-m4a" length="1234" href="gopher://umn.edu/greeting.m4a" />
  <author>
    <name>Kate Poster</name>
    <email>kate@example.com</email>
  </author>
  <updated>2007-03-17T00:00:00Z</updated>
  <published>2007-03-17T00:00:00Z</published>
  <content>
    <![CDATA[<div><p>Welcome to my blog.</p><ul><li>& ' < > © ℗ ™</li></ul></div>]]>
  </content>
</entry>
ATOM
    )
  (check-equal? (express-xml e1 'atom #f) expect-atom))

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

(module+ test
  (check-exn exn:fail:contract? (λ () (feed site-id "not a url" "Title" (list e1))))
  
  (define f1 (feed site-id "https://example.com/" "Kate Poster Posts" (list e1)))
  (define expect-feed-atom #<<ATOMFEED
<?xml version="1.0" encoding="UTF-8"?>

<feed xmlns="http://www.w3.org/2005/Atom">
  <title>Kate Poster Posts</title>
  <id>tag:example.com,2007:blog</id>
  <link rel="self" href="https://example.com/feed.atom" />
  <link rel="alternate" href="https://example.com/" />
  <updated>2007-03-17T00:00:00Z</updated>
  <entry>
    <title type="text">Kate&apos;s First Post</title>
    <id>tag:example.com,2007:blog.one</id>
    <link rel="alternate" href="https://example.com/blog/one.html" />
    <link rel="enclosure" type="audio/x-m4a" length="1234" href="gopher://umn.edu/greeting.m4a" />
    <author>
      <name>Kate Poster</name>
      <email>kate@example.com</email>
    </author>
    <updated>2007-03-17T00:00:00Z</updated>
    <published>2007-03-17T00:00:00Z</published>
    <content>
      <![CDATA[<div><p>Welcome to my blog.</p><ul><li>& ' < > © ℗ ™</li></ul></div>]]>
    </content>
  </entry>
</feed>
ATOMFEED
    )
  (define expect-feed-rss #<<RSSFEED
<?xml version="1.0" encoding="UTF-8"?>

<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>Kate Poster Posts</title>
    <description>Kate Poster Posts</description>
    <atom:link rel="self" href="https://example.com/feed.rss" type="application/rss+xml" />
    <link>https://example.com/</link>
    <pubDate>Sat, 17 Mar 2007 00:00:00 +0000</pubDate>
    <lastBuildDate>Sat, 17 Mar 2007 00:00:00 +0000</lastBuildDate>
    <item>
      <title>Kate's First Post</title>
      <author>kate@example.com (Kate Poster)</author>
      <link>https://example.com/blog/one.html</link>
      <guid isPermaLink="false">tag:example.com,2007:blog.one</guid>
      <enclosure url="gopher://umn.edu/greeting.m4a" length="1234" type="audio/x-m4a" />
      <pubDate>Sat, 17 Mar 2007 00:00:00 +0000</pubDate>
      <description>
        <![CDATA[<div><p>Welcome to my blog.</p><ul><li>& ' < > © ℗ ™</li></ul></div>]]>
      </description>
    </item>
  </channel>
</rss>
RSSFEED
    )
  (parameterize ([include-generator? #f])
    (check-equal? (express-xml f1 'atom "https://example.com/feed.atom") expect-feed-atom)
    (check-equal? (express-xml f1 'rss "https://example.com/feed.rss") expect-feed-rss))
  )

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