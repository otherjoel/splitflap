#lang scribble/manual

@(require
   "misc.rkt"
   scribble/examples
   (for-label splitflap 
              (except-in gregor date? date)
              (only-in net/url url?)
              racket/base 
              racket/promise
              (only-in racket/string non-empty-string?)
              txexpr
              xml)
   (for-syntax racket/base racket/syntax))

@title[#:tag "mod-splitflapp"]{Library Reference}

@(define mod-feed (make-base-eval #:lang 'racket/base))
@(mod-feed '(require splitflap))

@section{Building feeds}

Use @racket[feed-item] and @racket[feed] to create feeds for web content like blog posts, comments,
or even notifications: any content with a timestamp and its own URL.

You have a choice of using RSS or Atom formats, or both. Decades ago, holy wars were fought over
which format was superior, and it was safest to supply both in order to assure compatibility with
the most clients. These days almost every client supports both formats, so you probably only need to
supply one.

You should run all your feeds through the @W3CFeedValidator[]. Again, the design intent of this
package is that it should always throw an exception rather than produce a feed that would not pass
the validator. Please @hyperlink["https://github.com/otherjoel/splitflap/issues"]{file an issue}
should you encounter any validation errors in feeds created with Splitflap.


@defproc[(feed-item [id tag-uri?]
                    [url valid-url-string?]
                    [title string?]
                    [author person?]
                    [published moment?]
                    [updated moment?]
                    [content xexpr?]
                    [media (or/c enclosure? #f) #f])
         feed-item?]{

Returns a @racketresultfont{#<feed-item>} struct for inclusion in a @racket[feed]. You can inspect
its contents with @racket[express-xml].

The @racket[_id] argument must be a @tech{tag URI} (obtain from @racket[mint-tag-uri] or
@racket[append-specific]).

The @racket[_title] should not contain HTML markup; if it does, it will be escaped, and the raw markup may
by shown in applications that use your feed.

The @racket[_author] argument must be a @racket[person] struct.

The value of @racket[_updated] must be identical to or after @racket[_published], taking time zone
information into account, or an exception is raised. The values for these arguments can be most
conveniently supplied by @racket[infer-moment], but any moment-producing method will work, such as
constructing @racket[moment]s directly, parsing strings with @racket[parse-moment], etc.

If @racket[_content] is a @racketlink[txexpr?]{tagged X-expression}, it will be included as
XML-appropriate escaped HTML; if it is a plain string, it will be included as CDATA.

You can optionally use the @racket[_media] argument to supply an @tech{enclosure}, but if you are
generating a feed for a podcast you should consider using @racket[episode] and @racket[podcast]
instead.

}

@defproc[(feed [id tag-uri?]
               [site-url valid-url-string?]
               [name string?]
               [entries (listof feed-item?)])
          feed?]{

Returns a @racketresultfont{#<feed>} struct. You can inspect its contents with @racket[express-xml].

If any of the @tech{tag URIs} of the @racket[_entries] are @racket[tag=?] with each other or with the
feed @racket[_id], an exception is raised identifying the first duplicate encountered.

}

@section{Producing feed XML}

@defproc[(express-xml [data food?]
                      [dialect (or/c 'rss 'atom)]
                      [feed-url (or/c valid-url-string? #f) #f]
                      [#:as result-type (or/c 'xml-string 'xexpr 'xml) 'xml-string])
         (or/c string? txexpr? document? element?)]{

Returns the expression of @racket[_data] in one of three forms, depending on @racket[_result-type]:
a string of XML, a @racketlink[txexpr?]{tagged X-expression}, or an XML object. In the latter case,
the result is a @racket[document] when @racket[_data] is a @racket[feed] or a @racket[podcast], and
an @racket[element] otherwise.

The @racket[_dialect] argument is ignored when @racket[_data] is an @racket[episode] or a
@racket[podcast], since @AppleRequirements[] stipulate the use of RSS 2.0 for podcast feeds.

The @racket[_feed-url] argument must be supplied as a valid URL string when @racket[_data] is a
@racket[feed] or a @racket[podcast]; this should be the URL where the feed itself will be located.
It is not a required argument when @racket[_data] is any other type, and in those cases it will be
ignored if supplied.
 
For complete feeds (e.g., when @racket[_data] is a @racket[feed] or @racket[podcast]), the
entries/episodes will be sorted in reverse chronological order by their “updated” timestamps, and
the most recent such timestamp is used as the value for feed-level “last updated” and/or “published”
elements. If the feed contains no entries or episodes, these feed-level timestamps will use
@racket[now/moment].

The output of complete feeds can be further affected by other parameters (view their documentation
for more information):

@itemlist[#:style 'compact

@item{If @racket[include-generator?] is @racket[#t], a @tt{generator} element will be included.}

@item{If @racket[feed-language] is not @racket[#f], its value will be used as the language code for
the feed; otherwise the result of @racket[(force system-language)] is used.}

@item{If @racket[feed-xslt-stylesheet] is not @racket[#f], its value will be used as the path to a
stylesheet for the feed.}

]

@examples[#:eval mod-feed
          (define item
            (feed-item (mint-tag-uri "rclib.example.com" "2012-06" "blog:example-post")
                       "http://rclib.example.com"
                       "Example"
                       (person "Marion Paroo" "marion@rclib.example.com")
                       (infer-moment "2013-04-13 08:45")
                       (infer-moment "2013-04-14")
                       '(article (p "Etc…"))))
          (display (express-xml item 'atom #f))
          (express-xml item 'atom #:as 'xexpr)

          ]

}

@defparam[include-generator? incl? boolean? #:value #t]{

When set to @racket[#t], @racket[express-xml] will include a @tt{generator} element in the feed
generated for a @racket[feed] or @racket[podcast], naming @tt{Racket vN.n [cs/3m] / splitflap vN.n}
as the generator of the feed.

}

@defparam[feed-language lang (or/c iso-639-language-code? #f) #:value #f]{

A parameter that, when not set to @racket[#f], is used by @racket[express-xml] as the language for a
@racket[feed] or @racket[podcast] in place of @racket[system-language].

}

@defparam[feed-xslt-stylesheet path (or/c url? non-empty-string? #f) #:value #f]{

If this parameter is not @racket[#f], @racket[express-xml] will use its value as the path to an
@link["https://en.wikipedia.org/wiki/XSLT"]{XSLT stylesheet} in the prolog of the XML document
produced for a @racket[feed] or @racket[podcast].

Generally, the XSLT file must be served from the same domain as the feed itself, otherwise the
styling will not be applied.

@margin-note{Stylesheets can solve the problem of feeds looking and behaving in unfriendly ways when
accessed directly in a web browser. See
@link["https://github.com/genmon/aboutfeeds/issues/8#issuecomment-673293655"]{this Github issue} for
examples of people who have used XSLT stylesheets to add informative links and a welcoming layout to
their feeds.}

}

@section{Podcasts}

Splitflap provides some special data types for podcast feeds: @racket[episode] and
@racket[podcast]. These are patterned after @AppleRequirements[] since those serve as a kind of
de facto standard for this application.

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
                  [#:explicit? explicit any/c null]
                  [#:episode-num ep-num (or/c exact-nonnegative-integer? #f) #f]
                  [#:season-num s-num (or/c exact-nonnegative-integer? #f) #f]
                  [#:type type (or/c 'trailer 'full 'bonus #f) #f]
                  [#:block? block any/c #f])
         episode?]{

Returns an @racketresultfont{#<episode>} struct, which is required for @racket[podcast]s in the same
way that @racket[feed-item]s are required for @racket[feed]s. You can inspect its contents with
@racket[express-xml].

The value of @racket[_updated] must be identical to or after @racket[_published], taking time zone
information into account, or an exception is raised. The values for these arguments can be most
conveniently supplied by @racket[infer-moment], but any moment-producing method will work, such as
constructing @racket[moment]s directly, parsing strings with @racket[parse-moment], etc.

If @racket[_content] is a @racketlink[txexpr?]{tagged X-expression}, it will be included as escaped
HTML; if it is a plain string, it will be included as CDATA.

Below are further notes about particular elements supplied to @racket[episode]. The @spec{colored
passages} indicate things which are required by Apple for inclusion in the Apple Podcasts directory
but which are @emph{not} validated by Splitflap. (See @AppleRequirements[].)

@itemlist[

@item{The @racket[_title] should contain no markup, and @spec{should not contain episode or season
number information (use @racket[#:episode-num] and @racket[#:season-num] for this instead)}.}

@item{The @racket[#:image-url] is for episode-specific artwork. @spec{It must point to an image with
a minimum size of 1400 ⨉ 1400 pixels and a maximum size of 3000 ⨉ 3000 pixels (the preferred size),
in JPEG or PNG format, 72 dpi, with appropriate file extensions (.jpg, .png), and in the RGB
colorspace.} See @hyperlink["https://podcasters.apple.com/support/896-artwork-requirements"]{Apple
artwork requirements} for other requirements.}

@item{The @racket[#:duration] gives the episode’s duration in seconds, and is @spec{optional but
recommended}.}

@item{@racket[_explicit] is an optional override of the mandatory feed-level parental advisory flag
in @racket[podcast]. If it is @racket[_null] (the default), the episode will not contain any
parental advisory information. @spec{If it is @racket[#f], Apple Podcasts will mark the episode as
“Clean”. If it is any other value, Apple Podcasts will mark the episode as “Explicit”.}}

@item{The @racket[#:episode-num] is optional, but @spec{Apple will require it if the
@racket[podcast] has a type of @racket['episodic].}}

@item{You can optionally set @racket[#:type] to @racket['full] if this is a normal full-length
episode; to @racket['trailer] for short promos and previews; or to @racket['bonus] for extra or
cross-promotional content.}

@item{The @racket[#:block] flag can be set to anything other than @racket[#f] (the default) to
prevent a particular episode from appearing in Apple podcasts. For example you might want to block a
specific episode if you know that its content would otherwise cause the entire podcast to be removed
from Apple Podcasts.}

]

}

@defproc[(podcast [id tag-uri?]
                  [site-url valid-url-string?]
                  [name string?]
                  [episodes (listof episode?)]
                  [category (or/c string? (list/c string? string?))]
                  [image-url valid-url-string?]
                  [owner person?]
                  [#:explicit? explicit any/c]
                  [#:type type (or/c 'serial 'episodic #f) #f]
                  [#:block? block any/c #f]
                  [#:complete? complete any/c #f]
                  [#:new-feed-url new-url (or/c valid-url-string? #f) #f])
         podcast?]{

Returns a @racketresultfont{#<podcast>} struct, which can be converted into a feed with
@racket[express-xml].

If any of the @tech{tag URIs} of the @racket[_episodes] are @racket[tag=?] with each other or with
the podcast feed @racket[_id], an exception is raised identifying the first duplicate encountered.

Below are some notes about particular elements supplied to @racket[podcast]. The @spec{colored
passages} indicate things which are required by Apple for inclusion in the Apple Podcasts directory
but which are @emph{not} validated by Splitflap. (See @AppleRequirements[].)

@itemlist[

@item{The @racket[_category] can be either a simple category or a list containing a category and
sub-category. @spec{The category names must be drawn from
@hyperlink["https://podcasters.apple.com/support/1691-apple-podcasts-categories"]{Apple’s podcast
category list}.}}

@item{The @racket[_image-url] links to the show’s artwork. @spec{It must point to an image with
a minimum size of 1400 ⨉ 1400 pixels and a maximum size of 3000 ⨉ 3000 pixels (the preferred size),
in JPEG or PNG format, 72 dpi, with appropriate file extensions (.jpg, .png), and in the RGB
colorspace.} See @hyperlink["https://podcasters.apple.com/support/896-artwork-requirements"]{Apple
artwork requirements} for other requirements.}

@item{The @racket[_owner] is for administrative communication about the podcast and is not displayed
in Apple’s podcast listings.}

@item{The @racket[#:type] can be one of two values.

@itemlist[

@item{Use @racket['episodic] when episodes are not intended to be consumed in any particular order:
@spec{in this case, Apple Podcasts will present newest episodes first. (If organized into seasons,
the newest season will be presented first; otherwise, episodes will be grouped by year published,
newest first.)}}

@item{Use @racket['serial] when
episodes are intended to be consumed in sequential order: @spec{in this case, Apple Podcasts will
present the oldest episodes first. (If organized into seasons, the newest season will be shown
first.)}}

]}

@item{Setting @racket[#:block] to a value other than @racket[#f] will prevent the entire podcast
from appearing in Apple Podcasts.}

@item{Setting @racket[#:complete?] to a value other than @racket[#f] indicates that a podcast is
complete and you will not post any more episodes in the future.}

@item{If you change the URL where your feed is located, then (when generating the feed located at
the original URL) set @racket[#:new-feed-url] to the new feed’s URL. Apple Podcasts (and possibly
other clients) will automatically update subscriptions to use the new feed. See
@hyperlink["https://podcasters.apple.com/support/837-change-the-rss-feed-url"]{Apple’s guidelines
for moving your RSS feed}.}

]}

@section{Feed type predicates}

@defproc[(feed-item? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a @racket[feed-item] struct, @racket[#f] otherwise.

}

@defproc[(feed? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a @racket[feed] struct, @racket[#f] otherwise. 

}

@defproc[(episode? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is an @racket[episode] struct, @racket[#f] otherwise.

}

@defproc[(podcast? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a @racket[podcast] struct, @racket[#f] otherwise.

}

@defproc[(food? [v any/c]) boolean?]{

Returns @racket[#t] when @racket[_v] is one of the struct types that implements support for the
generic @racket[express-xml] function: @racket[enclosure], @racket[feed-item], @racket[feed],
@racket[episode], or @racket[podcast]; @racket[#f] otherwise}
