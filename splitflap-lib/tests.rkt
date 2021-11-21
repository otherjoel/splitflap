#lang at-exp racket/base

;; Unit tests

(require "main.rkt"
         gregor
         racket/format
         rackunit)

(define site-id (mint-tag-uri "example.com" "2007" "blog"))
(define e1
  (parameterize ([current-timezone 0])
    (feed-item (append-specific site-id "one")
                "https://example.com/blog/one.html"
                "Kate's First Post"
                (person "Kate Poster" "kate@example.com")
                (infer-moment "2007-03-17")
                (infer-moment "2007-03-17")
                `(div (p "Welcome to my blog.") (ul (li "& ' < > © ℗ ™")))
                (enclosure "gopher://umn.edu/greeting.m4a" "audio/x-m4a" 1234))))

(define expect-atom @~a{
<entry>
  <title type="text">Kate's First Post</title>
  <link rel="alternate" href="https://example.com/blog/one.html" />
  <updated>2007-03-17T00:00:00Z</updated>
  <published>2007-03-17T00:00:00Z</published>
  <link rel="enclosure" type="audio/x-m4a" length="1234" href="gopher://umn.edu/greeting.m4a" />
  <author>
    <name>Kate Poster</name>
    <email>kate@"@"example.com</email>
  </author>
  <id>tag:example.com,2007:blog.one</id>
  <content>
    <![CDATA[<div><p>Welcome to my blog.</p><ul><li>& ' < > © ℗ ™</li></ul></div>]]>
  </content>
</entry>})

(check-equal? (express-xml e1 'atom) expect-atom)

;; Updated time occurring before published time results in exception
(check-exn
 exn:fail:contract?
 (λ () (feed-item (append-specific site-id "one")
                "https://example.com/blog/one.html"
                "Kate's First Post"
                (person "Kate Poster" "kate@example.com")
                (infer-moment "2007-03-17")
                (infer-moment "2007-03-16")
                `(div (p "Welcome to my blog.") (ul (li "& ' < > © ℗ ™")))
                (enclosure "gopher://umn.edu/greeting.m4a" "audio/x-m4a" 1234))))

(check-exn exn:fail:contract? (λ () (feed site-id "not a url" "Title" (list e1))))
  
(define f1 (feed site-id "https://example.com/" "Kate Poster Posts" (list e1)))
(define expect-feed-atom @~a{
<?xml version="1.0" encoding="UTF-8"?>

<feed xmlns="http://www.w3.org/2005/Atom" xml:lang="en">
  <title>Kate Poster Posts</title>
  <link rel="self" href="https://example.com/feed.atom" />
  <link rel="alternate" href="https://example.com/" />
  <updated>2007-03-17T00:00:00Z</updated>
  <id>tag:example.com,2007:blog</id>
  <entry>
    <title type="text">Kate's First Post</title>
    <link rel="alternate" href="https://example.com/blog/one.html" />
    <updated>2007-03-17T00:00:00Z</updated>
    <published>2007-03-17T00:00:00Z</published>
    <link rel="enclosure" type="audio/x-m4a" length="1234" href="gopher://umn.edu/greeting.m4a" />
    <author>
      <name>Kate Poster</name>
      <email>kate@"@"example.com</email>
    </author>
    <id>tag:example.com,2007:blog.one</id>
    <content>
      <![CDATA[<div><p>Welcome to my blog.</p><ul><li>& ' < > © ℗ ™</li></ul></div>]]>
    </content>
  </entry>
</feed>})

(define expect-feed-rss @~a{
<?xml version="1.0" encoding="UTF-8"?>

<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>Kate Poster Posts</title>
    <atom:link rel="self" href="https://example.com/feed.rss" type="application/rss+xml" />
    <link>https://example.com/</link>
    <pubDate>Sat, 17 Mar 2007 00:00:00 +0000</pubDate>
    <lastBuildDate>Sat, 17 Mar 2007 00:00:00 +0000</lastBuildDate>
    <description>Kate Poster Posts</description>
    <language>en</language>
    <item>
      <title>Kate's First Post</title>
      <link>https://example.com/blog/one.html</link>
      <pubDate>Sat, 17 Mar 2007 00:00:00 +0000</pubDate>
      <enclosure url="gopher://umn.edu/greeting.m4a" length="1234" type="audio/x-m4a" />
      <author>kate@"@"example.com (Kate Poster)</author>
      <guid isPermaLink="false">tag:example.com,2007:blog.one</guid>
      <description>
        <![CDATA[<div><p>Welcome to my blog.</p><ul><li>& ' < > © ℗ ™</li></ul></div>]]>
      </description>
    </item>
  </channel>
</rss>})

(parameterize ([include-generator? #f]
               [feed-language 'en])
  (check-equal? (express-xml f1 'atom "https://example.com/feed.atom") expect-feed-atom)
  (check-equal? (express-xml f1 'rss "https://example.com/feed.rss") expect-feed-rss))

(define test-ep1
  (parameterize ([current-timezone 0])
  (episode (append-specific site-id "ep1")
           "http://example.com/ep1"
           "Kate Speaks"
           (person "Kate Poster" "kate@example.com")
           (infer-moment "2021-10-31")
           (infer-moment "2021-10-31")
           `(article (p "Welcome to the show"))
          (enclosure "http://example.com/ep1.m4a" "audio/x-m4a" 1234)
           )))

(define expect-ep1 @~a{
<item>
  <title>Kate Speaks</title>
  <link>http://example.com/ep1</link>
  <pubDate>Sun, 31 Oct 2021 00:00:00 +0000</pubDate>
  <enclosure url="http://example.com/ep1.m4a" length="1234" type="audio/x-m4a" />
  <author>kate@"@"example.com (Kate Poster)</author>
  <guid isPermaLink="false">tag:example.com,2007:blog.ep1</guid>
  <description>
    <![CDATA[<article><p>Welcome to the show</p></article>]]>
  </description>
</item>
})

(check-equal? expect-ep1 (express-xml test-ep1 'rss))
(check-equal? expect-ep1 (express-xml test-ep1 'atom)) ; ignores dialect
                                        ;
(define test-ep2
  (parameterize ([current-timezone 0])
  (episode (append-specific site-id "ep2")
           "http://example.com/ep2"
           "Kate Speaks"
           (person "Kate Poster" "kate@example.com")
           (infer-moment "2021-10-31")
           (infer-moment "2021-10-31")
           `(article (p "Welcome to another show"))
          (enclosure "http://example.com/ep2.m4a" "audio/x-m4a" 1234)
          #:duration 300
          #:image-url "http://example.com/ep1-cover.png"
          #:explicit? (even? 7)
          #:episode-num 2
          #:season-num 1
          #:type 'full
          #:block? (< 3 17 99)
           )))

(define expect-ep2 @~a{
<item>
  <title>Kate Speaks</title>
  <link>http://example.com/ep2</link>
  <pubDate>Sun, 31 Oct 2021 00:00:00 +0000</pubDate>
  <enclosure url="http://example.com/ep2.m4a" length="1234" type="audio/x-m4a" />
  <itunes:episodeType>full</itunes:episodeType>
  <author>kate@"@"example.com (Kate Poster)</author>
  <guid isPermaLink="false">tag:example.com,2007:blog.ep2</guid>
  <description>
    <![CDATA[<article><p>Welcome to another show</p></article>]]>
  </description>
  <itunes:duration>300</itunes:duration>
  <itunes:explicit>false</itunes:explicit>
  <itunes:image href="http://example.com/ep1-cover.png" />
  <itunes:episode>2</itunes:episode>
  <itunes:season>1</itunes:season>
  <itunes:block>Yes</itunes:block>
</item>
})

(check-equal? expect-ep2 (express-xml test-ep2 'rss))
(check-equal? expect-ep2 (express-xml test-ep2 'atom)) ; ignores dialect
