#lang racket/base

(require "constructs.rkt"
         "private/dust.rkt"
         "private/xml-generic.rkt"
         gregor
         txexpr
         racket/contract
         racket/generic
         racket/match
         racket/port
         xml)

(struct feed-entry
  (id url title author published updated content)
  #:guard (struct-guard/c
           tag-uri?
           valid-url-string?
           string?
           person?
           moment?
           moment?
           xexpr?)
  
  #:methods gen:xml-source
  [(define/contract (express-xml e dialect url #:as [result-type 'xml-string])
     (->* (any/c rss-dialect? any/c) (#:as xml-type/c) any/c)
     
     (match-define (feed-entry id url title author published updated content) e)
     (define xml? (memq result-type '(xml xml-string xexpr-cdata)))
     (define entry-xpr
       (case dialect
         [(atom)
          `(entry
            (title [[type "text"]] ,title)
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
            (link ,url)
            (guid [[isPermaLink "false"]] ,(tag-uri->string id))
            (pubDate ,(moment->string updated 'rss))
            (description ,(if xml? (as-cdata content) (cdata-string (as-cdata content)))))]))
     (case result-type
       [(xexpr xexpr-cdata) entry-xpr]
       [(xml) (xexpr->xml entry-xpr)]
       [(xml-string) (indented-xml-string entry-xpr)]))])

(module+ test
  (require rackunit)
  (define site-id (mint-tag-uri "example.com" "2007" "blog"))
  (define e1
    (parameterize ([current-timezone 0])
      (feed-entry (append-specific site-id "one")
                  "https://example.com/blog/one.html"
                  "Kate's First Post"
                  (person "Kate Poster" "kate@example.com")
                  (infer-moment "2007-03-17")
                  (infer-moment "2007-03-17")
                  "Hi!")))

  (define expect-atom #<<ATOM
<entry>
  <title type="text">Kate&apos;s First Post</title>
  <id>tag:example.com,2007:blog.one</id>
  <link rel="alternate" href="https://example.com/blog/one.html" />
  <author>
    <name>Kate Poster</name>
    <email>kate@example.com</email>
  </author>
  <updated>2007-03-17T00:00:00Z</updated>
  <published>2007-03-17T00:00:00Z</published>
  <content>
    <![CDATA[Hi!]]>
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

  #:methods gen:xml-source
  [(define/generic super-express-xml express-xml)
   (define/contract (express-xml f dialect feed-url #:as [result-type 'xml-string])
     (->* (any/c rss-dialect? valid-url-string?) (#:as xml-type/c) (or/c string? document? txexpr?))

     (match-define (feed feed-id site-url feed-name entries) f)
     (define entries-sorted (sort entries entry-newer?))
     (define last-updated (feed-entry-updated (car entries-sorted)))
     (define xml? (memq result-type '(xml xml-string)))
  
     (define feed-xpr
       (case dialect
         [(atom)
          `(feed [[xmlns "http://www.w3.org/2005/Atom"]]
                 (title ,feed-name)
                 ,@(if (include-generator?) (list (generator 'atom)) '())
                 (id ,(tag-uri->string feed-id))
                 (link [[rel "self"] [href ,feed-url]])
                 (link [[rel "alternate"] [href ,site-url]])
                 (updated ,(moment->string last-updated 'atom))
                 ,@(for/list ([e (in-list entries-sorted)])
                     (super-express-xml e 'atom #f #:as (if xml? 'xexpr-cdata 'xexpr))))]
         [(rss)
          `(rss [[version "2.0"] [xmlns:atom "http://www.w3.org/2005/Atom"]]
                (channel
                 (title ,feed-name)
                 ,@(if (include-generator?) (list (generator 'rss)) '())
                 (description ,feed-name)
                 (atom:link [[rel "self"] [href ,feed-url] [type "application/rss+xml"]])
                 (link ,site-url)
                 (pubDate ,(moment->string last-updated 'rss))
                 (lastBuildDate ,(moment->string last-updated 'rss))
                 ,@(for/list ([e (in-list entries-sorted)])
                     (super-express-xml e 'rss #f #:as (if xml? 'xexpr-cdata 'xexpr)))))]))
     (case result-type
       [(xexpr) feed-xpr]
       [(xml) (xml-document feed-xpr)]
       [(xml-string) (indented-xml-string feed-xpr #:document? #t)]))])

(module+ test
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
    <author>
      <name>Kate Poster</name>
      <email>kate@example.com</email>
    </author>
    <updated>2007-03-17T00:00:00Z</updated>
    <published>2007-03-17T00:00:00Z</published>
    <content>
      <![CDATA[Hi!]]>
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
      <pubDate>Sat, 17 Mar 2007 00:00:00 +0000</pubDate>
      <description>
        <![CDATA[Hi!]]>
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