#lang scribble/manual

@(require
   scribble/examples
   (for-label splitflap (except-in gregor date? date) xml racket/base racket/promise)
   (for-syntax racket/base racket/syntax))

@title[#:tag "mod-splitflapp"]{Library Reference}

@(define mod-feed (make-base-eval #:lang 'racket/base))
@(mod-feed '(require splitflap))

@section{Building feeds}

Use @racket[feed-item] and @racket[feed] to create feeds for web content like blog posts, comments,
or even notifications: any content with a timestamp and its own URL.

You have a choice of using RSS or Atom formats, or both. Twenty years ago, holy wars were fought
over which format was superior and it was necessary to supply both in order to assure compatibility
with the most clients. As of 2021 almost every client supports both, so you probably only need to
supply one.

@defproc[(feed-item [id tag-uri?]
                    [url valid-url-string?]
                    [title string?]
                    [author person?]
                    [published moment?]
                    [updated moment?]
                    [content xexpr?]
                    [media (or/c enclosure? #f) #f])
         feed-item?]{

Returns an opaque @racketresultfont{#<feed-item>} struct for inclusion in a @racket[feed].

The value of @racket[_id] must be a value returned from @racket[mint-tag-uri] or
@racket[append-specific].

The value of @racket[_author] must be a value returned from @racket[person].

The values of @racket[_published] and @racket[_updated] can be most conveniently supplied by
@racket[infer-moment]. You can also use functions in the @other-doc['(lib
"gregor/scribblings/gregor.scrbl")] library which return @racket[moment]s, such as
@racket[parse-moment].

}

@defproc[(feed [id tag-uri?]
                [site-url valid-url-string?]
                [name xexpr?]
                [entries (listof feed-item?)])
          feed?]{

Returns an opaque @racketresultfont{#<feed>} struct. The only way to inspect the data in the
resulting struct is to pass it to @racket[express-xml].

The @racket[_entries] will be sorted in reverse chronological order by their “updated” timestamps.
The most recent timestamp is also used as the feed’s own last-updated timestamp.

}

@section{Producing feed XML}

@defproc[(express-xml [data food?]
                      [dialect (or/c 'rss 'atom)]
                      [feed-url (or/c valid-url-string? #f) #f]
                      [#:as result-type (or/c 'xml-string 'xexpr 'xml) 'xml-string])
         (or/c string? xexpr? document? element?)]{

Returns the expression of @racket[_data] in one of three forms, depending on @racket[_result-type]:
a string of XML; an @racketlink[xexpr?]{X-expression}; or an XML object. In the latter case, the
result is an @racket[element] when @racket[_data] is a @racket[feed-item] or an @racket[episode],
and a @racket[document] when @racket[_data] is a @racket[feed] or a @racket[podcast].

The @racket[_dialect] argument is ignored when @racket[_data] is an @racket[episode] or a
@racket[podcast], since Apple’s podcast requirements stipulate the use of RSS 2.0 for podcast feeds.

The @racket[_feed-url] argument must be supplied as a valid URL string when @racket[_data] is a
@racket[feed] or a @racket[podcast]; this should be the URL where the feed itself will be located.
It is not a required argument when @racket[_data] is any other type, and in those cases it will be
ignored if supplied.
 
If the parameter @racket[include-generator?] is @racket[#t], a @tt{generator} element will be included
in complete feeds. If the parameter @racket[feed-language] is not set to @racket[#f], it will be used
as the language code for the feed; otherwise the result of @racket[(force system-language)] is used.

@examples[#:eval mod-feed
          (define item
            (feed-item (mint-tag-uri "rclib.org" "2012-06" "blog:example-post")
                       "http://rclib.org/example-post.html"
                       "Example"
                       (person "Marion Paroo" "marion@rclib.org")
                       (infer-moment "2013-04-13 08:45")
                       (infer-moment "2013-04-14")
                       '(article (p "Etc…"))))
          (display (express-xml item 'atom #f))
          (express-xml item 'atom #:as 'xexpr)

          ]

}

@defparam[include-generator? incl? boolean? #:value #t]{

When set to @racket[#t], @racket[express-xml] will a @tt{generator} element in the feed generated
for a @racket[feed] or @racket[podcast], naming @tt{Racket vN.n [cs/3m] / splitflap vN.n} as the
generator of the feed.

}

@defparam[feed-language lang (or/c iso-639-language-code? #f) #:value #f]{

A parameter that, when not set to @racket[#f], is used in place of @racket[system-language] as the language
for a feed by @racket[express-xml].

}

@section{Podcasts}

Splitflap adheres to Apple’s podcast requirements as much as possible.

@defproc[(episode [id tag-uri?]
                  [url valid-url-string?]
                  [title string?]
                  [author person?]
                  [published moment?]
                  [updated moment?]
                  [content xexpr?]
                  [media enclosure?]
                  [#:duration duration (or/c exact-nonnegative-integer? #f) #f]
                  [#:image-url image-url (or/c valid-url-string? #f) #f]
                  [#:explicit? explicit (or/c boolean? '_) '_]
                  [#:episode-num ep-num (or/c exact-nonnegative-integer? #f) #f]
                  [#:season-num s-num (or/c exact-nonnegative-integer? #f) #f]
                  [#:type type (or/c 'trailer 'full 'bonus #f) #f]
                  [#:block? block boolean? #f])
         episode?]{

Returns an opaque struct of type @racketresultfont{#<episode>} suitable for inclusion in a
@racket[podcast].

}

@defproc[(podcast [id tag-uri?]
                  [site-url valid-url-string?]
                  [name xexpr?]
                  [episodes (listof episode?)]
                  [category (or/c string? (list/c string? string?))]
                  [image-url valid-url-string?]
                  [owner person?]
                  [explicit boolean?]
                  [#:type type (or/c 'serial 'episodic #f) #f]
                  [#:block? block boolean? #f]
                  [#:complete? complete boolean? #f]
                  [#:new-feed-url new-url (or/c valid-url-string? #f) #f])
         podcast?]{

Returns an opaque struct of type @racketresultfont{#<podcast>}.

}

@section{Feed type predicates}

@defproc[(feed-item? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a @racketresultfont{#<feed-item>} struct --- that is, the
result of a call to @racket[feed-item].

}

@defproc[(feed? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a @racketresultfont{#<feed>} struct --- that is, the result of
a call to @racket[feed].

}

@defproc[(episode? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is an @racketresultfont{#<episode>} struct --- that is, the
result of a call to @racket[episode].

}

@defproc[(podcast? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is an @racketresultfont{#<podcast>} struct --- that is, the
result of a call to @racket[podcast].

}

@defproc[(food? [v any/c]) boolean?]{

Returns @racket[#t] when @racket[_v] is one of the struct types that implements the generic
@racket[express-xml] function: @racket[enclosure], @racket[feed-item], @racket[feed],
@racket[episode], or @racket[podcast].

}
