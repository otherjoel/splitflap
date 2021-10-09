#lang racket/base

;; Unit tests

(require "main.rkt" gregor rackunit)

(define site-id (mint-tag-uri "example.com" "2007" "blog"))
(define e1
  (parameterize ([current-timezone 0])
    (feed-entry (append-specific site-id "one")
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
(check-equal? (express-xml e1 'atom #f) expect-atom)

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
