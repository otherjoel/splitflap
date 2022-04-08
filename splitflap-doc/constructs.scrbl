#lang scribble/manual

@(require "misc.rkt"
          scribble/examples
          (only-in scribble/eval interaction)
          (only-in scribble/bnf nonterm)
          (for-label (except-in gregor date date?)
                     net/url
                     racket/base
                     racket/contract
                     racket/file
                     racket/promise
                     racket/string
                     splitflap
                     txexpr))

@title[#:tag "mod-constructs"]{Feed Constructs}

@(define mod-constructs (make-base-eval #:lang 'racket/base))
@(mod-constructs '(require splitflap gregor racket/file racket/promise))

@defmodule[splitflap/constructs]

The format of feeds is specified by the @Atom1.0[] and @RSS2.0[] specifications (and, for all
practical purposes, by @AppleRequirements[] in the case of podcasts). These in turn reference other
RFCs to specify the format of many individual elements: timestamps, domain names, email addresses,
people, identifiers, and languages.

Splitflap makes heavy use of custom @seclink["contract-boundaries" #:doc '(lib
"scribblings/guide/guide.scrbl")]{contracts} to ensure conformity to the spec at every level. In
cases where it makes things simpler, Splitflap is a bit @emph{more} strict than the actual spec.

The bindings documented in this section are provided by the main @racketmodname[splitflap] module
as well as by @racketmodname[splitflap/constructs].

@section{Tag URIs}

Feeds, and items contained in feeds, require some globally unique identifier. Although any kind of
reasonably unique identifier can be used in a feed, Splitflap takes the unreasonably opinionated
stance of allowing only @tech{tag URIs}, which are easy to create and read, and which can remain
stable even if the resource’s URL changes.

A @deftech{tag URI} is an identifier of the form
@racketvalfont{@litchar{tag:}@nonterm{authority}@litchar{,}@nonterm{date}@litchar{:}@nonterm{specific}}.
The @nonterm{authority} is a domain name (or email address) held by you as of @nonterm{date};
together, the authority and the date form a unique @italic{tagging entity}, which acts kind of like
a namespace. The @nonterm{specific} is a string uniquely identifying a particular resource within
the tagging entity.

The tag URI scheme is formalized in @hyperlink["https://datatracker.ietf.org/doc/html/rfc4151"]{RFC
4151}.

@defproc[(mint-tag-uri [authority (or/c dns-domain? email-address?)]
                       [date tag-entity-date?]
                       [specific tag-specific-string?])
         tag-uri?]{

Returns a @tech{tag URI} struct for use as a unique identifier in a @racket[feed-item],
@racket[feed], @racket[@episode] or @racket[podcast].

The @racket[_date] must be any date on which you had ownership or assignment of the domain or
email address at 00:00 UTC (the start of the day). (See @racket[tag-entity-date?].)

The @racket[_specific] is a string that must be reliably and permanently unique within the set of
things that your feed is serving. See @racket[tag-specific-string?] for information about what
characters are allowed here.

@examples[#:eval mod-constructs
          (mint-tag-uri "rclib.example.com" "2012-04-01" "Marian'sBlog")
          (mint-tag-uri "diveintomark.example.com" "2003" "3.2397")]

}

@defproc[(tag-uri->string [tag tag-uri?]) non-empty-string?]{

Converts a @racketlink[mint-tag-uri]{@tt{tag-uri}} into a string.

@examples[#:eval mod-constructs
          (define rclib-id (mint-tag-uri "rclib.example.com" "2012-04-01" "Marian'sBlog"))
          (tag-uri->string rclib-id)]

}

@defproc[(append-specific [tag tag-uri?] [suffix tag-specific-string?]) tag-uri?]{

Returns a copy of @racket[_tag] with @racket[_suffix] appended to the “specific” (last) portion of
the tag URI.  This allows you to append to a feed’s @tech{tag URI} to create unique identifiers for
the items within that feed.

@examples[#:eval mod-constructs
          (define kottke-id (mint-tag-uri "kottke.example.com" "2005-12" "1"))
          kottke-id
          (append-specific kottke-id "post-slug")]

}

@defproc[(tag=? [tag1 tag-uri?] [tag2 tag-uri?]) boolean?]{

@margin-note{The tag URI spec
@hyperlink["https://datatracker.ietf.org/doc/html/rfc4151#section-2.4"]{defines} tags as being equal
when their byte-strings are indistinguishable.}

Returns @racket[#t] if the @racket[tag-uri->string] representation of @racket[_tag1] and
@racket[_tag2] are @racket[equal?], @racket[#f] otherwise.

}

@defproc[(tag-entity-date? [str string?]) boolean?]{

Returns @racket[#t] if @racket[_str] is a string of the form @racket{YYYY[-MM[-DD]]} --- that is, an
acceptable date format for a @tech{tag URI} according to RFC 4151.

@examples[#:eval mod-constructs
          @(code:comment @#,elem{Equivalent to January 1, 2012})
          (tag-entity-date? "2012")
          @(code:comment @#,elem{Equivalent to June 1, 2012})
          (tag-entity-date? "2012-06")
          @(code:comment @#,elem{take a guess on this one})
          (tag-entity-date? "2012-10-21")
          (tag-entity-date? "2012-1-1")]
}

@defproc[(tag-specific-string? [str string?]) boolean?]{

Returns @racket[#t] if @racket[_str] is an acceptable string for the “specific” portion of a
@tech{tag URI} as specified in RFC 4151: a string comprised only of the characters in the range
@litchar{a–z}, @litchar{A–Z}, @litchar{0–9} or in the set @litchar{-._~!$&'()*+,;=:@"@"/?}.

@examples[#:eval mod-constructs
          (tag-specific-string? "abcdABCD01923")
          (tag-specific-string? "-._~!$&'()*+,;=:@/?")
          (tag-specific-string? "")
          (tag-specific-string? "^")]

}

@defproc[(tag-uri? [v any/c]) boolean?]{

Returns @racket[#t] when @racket[_v] is a @racketlink[mint-tag-uri]{@tt{tag-uri}} struct.

}

@section{Persons}

@defproc[(person [name non-empty-string?]
                 [email email-address?]
                 [url (or/c valid-url-string? #f) #f])
         person?]{

Returns a @racketresultfont{#<person>} struct for use in a @racket[feed-item], @racket[feed],
@racket[episode] or @racket[podcast].
                  
The @Atom1.0[] and @RSS2.0[] specs both have opinions about how people should be referenced in
feeds. Atom requires only a name but also allows up to one email address and up to one URI. RSS
requires one email address optionally followed by anything. So @racket[person] requires both a
@racket[_name] and an @racket[_email], and the @racket[_url] is optional.

}

@defproc[(person->xexpr [p person?] [entity symbol?] [dialect (or/c 'rss 'atom 'itunes)]) txexpr?]{

Converts @racket[_p] into a tagged X-expresssion using @racket[_entity] as enclosing tag name.

@examples[#:eval mod-constructs
          (define frank (person "Frankincense Pontipee" "frank@example.com"))
          (person->xexpr frank 'author 'atom)
          (person->xexpr frank 'contributor 'atom)
          (person->xexpr frank 'author 'rss)
          (person->xexpr frank 'itunes:owner 'itunes)]

}

@defproc[(person? [v any/c]) boolean?]{

Returns @racket[#t] when @racket[_v] is a @racket[person] struct, @racket[#f] otherwise.

}

@section{Date and time information}

Feeds and feed items must be timestamped, and these values must include timezone information.
Splitflap leans on the @racketmodname[gregor] library for this functionality --- in particular,
@secref["moment" #:doc '(lib "gregor/scribblings/gregor.scrbl")] and @secref["timezone" #:doc '(lib
"gregor/scribblings/gregor.scrbl")] --- and provides a couple of helper functions to make things a
bit more ergonomic.

@defproc[(infer-moment [str string? ""]) moment?]{

Parses from @racket[_str] and returns a precise @racket[moment], inferring time information where
ommitted and using @racket[current-timezone] as the time zone for the moment.

If @racket[_str] is @racketvalfont{""}, then the result of @racket[now/moment] is returned.
Otherwise @racket[_str] must be in the form @racket{YYYY-MM-DD [hh:mm[:ss]]} or an exception is
raised. If the seconds are ommitted, @racketvalfont{00} is assumed, and if the hours and minutes are
ommitted, @racketvalfont{00:00:00} (the very start of the date) is assumed.

@examples[#:eval mod-constructs
          (infer-moment "2012-08-31")
          (infer-moment "2012-08-31 13:34")
          (infer-moment "2015-10-02 01:03:15")

          (parameterize ([current-timezone -14400])
            (infer-moment "2015-10-02 01:03:15"))

          (infer-moment "2012-09-14 12")
          (infer-moment)]

@history[#:changed "1.2" "Added no-argument form for current moment"]

}

@defproc[(moment->string [m moment?] [dialect (or/c 'atom 'rss)]) non-empty-string?]{

Converts @racket[_m] into a timestamp in the format required by the chosen @racket[_dialect]:
@hyperlink["https://datatracker.ietf.org/doc/html/rfc3339"]{RFC 3339} for Atom and
@hyperlink["https://www.rfc-editor.org/rfc/rfc822.html"]{RFC 822} for RSS.

@examples[#:eval mod-constructs
          (define m1 (infer-moment "2012-10-01"))
          (moment->string m1 'atom)
          (moment->string m1 'rss)
          (parameterize ([current-timezone 0])
            (moment->string (infer-moment "2012-10-01") 'atom))]
            
}

@section{Enclosures and MIME types}

An @deftech{enclosure} is an arbitrary resource related to a feed item that is potentially large in
size and may require special handling. The canonical example is an MP3 file containing the audio for
a podcast episode.

@defstruct[enclosure ([url valid-url-string?] 
                      [mime-type (or/c non-empty-string? #f)] 
                      [size exact-nonnegative-integer?]) #:omit-constructor]{

A structure type for @tech{enclosures}.

The @racket[_mime-type], if provided and not set to @racket[#f], must be a useable MIME type, but is
not currently validated to ensure this. The @racket[_size] should be the resource’s size in bytes.

This struct qualifies as @racketlink[food?]{food}, so it can be converted to XML with
@racket[express-xml].

}

@defproc[(file->enclosure [file path-string?] [base-url valid-url-string?]) enclosure?]{

Returns an @racket[enclosure] for @racket[_file], with a MIME type matching the file’s extension (if
it can be determined), the URL set to @racket[_file] appended onto @racket[_base-url], and the
length set to the file’s actual length in bytes.

This procedure accesses the filesystem; if @racket[_file] does not exist, an exception is raised.

@examples[#:eval mod-constructs
          (code:comment @#,elem{Make a temporary file})
          (define audio-file (make-temporary-file "audio-~a.m4a"))
          (display-to-file (make-bytes 100 66) audio-file #:exists 'truncate)
          (code:comment @#,elem{Pass the temp file to an enclosure})
          (display
           (express-xml (file->enclosure audio-file "http://example.com") 'atom))
          (code:comment @#,elem{Cleanup})
          (delete-file audio-file)]
}

@defthing[mime-types-by-ext (promise/c (hash/c symbol? string?))]{

@margin-note{This table is built directly from
@hyperlink["https://svn.apache.org/viewvc/httpd/httpd/trunk/docs/conf/mime.types?view=markup"]{the
list maintained in the Apache SVN repository}.}

A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{promise} that, when @racket[force]d,
yields a hash table mapping file extensions (in lowercase symbol form) to MIME types.

@examples[#:eval mod-constructs
          (hash-ref (force mime-types-by-ext) 'epub)]

}

@defproc[(path/string->mime-type [path path-string?]) (or/c string? #f)]{

Parses a file extension from @racket[_path] and returns its corresponding MIME type if one exists
in @racket[mime-types-by-ext], @racket[#f] otherwise. This function does not access the file system.

@examples[#:eval mod-constructs
          (path/string->mime-type ".m4a")
          (path/string->mime-type "SIGIL_v1_21.wad")
          (code:line (path/string->mime-type "mp3") (code:comment "No period, so no file extension!"))]

}

@section{Domains, URLs and email addresses}

@defproc[(dns-domain? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a string whose entire contents are a valid DNS domain
according to @hyperlink["https://datatracker.ietf.org/doc/html/rfc1035"]{RFC 1035}:

@itemlist[

@item{Must contain one or more @emph{labels} separated by @litchar{.}}

@item{Each label must consist of only the characters @litchar{A–Z}, @litchar{a–z}, @litchar{0–9}, or
@litchar{-}.}

@item{Labels may not start with a digit or a hyphen, and may not end in a hyphen.}

@item{No individual label may be longer than 63 bytes (including an extra byte for a length header),
and the entire domain may not be longer than 255 bytes.}

]

@examples[#:eval mod-constructs
          (dns-domain? "a")
          (dns-domain? "rclib.org")
          (dns-domain? "a.b.c.d.e-f")
          (dns-domain? "a.b1000.com")
          code:blank
          (define longest-valid-label (make-string 62 #\a))
          (define longest-valid-domain
            (string-append longest-valid-label "." (code:comment @#,elem{63 bytes (including length header)})
                           longest-valid-label "." (code:comment @#,elem{126})
                           longest-valid-label "." (code:comment @#,elem{189})
                           longest-valid-label "." (code:comment @#,elem{252})
                           "aa"))                  (code:comment @#,elem{255 bytes})
          code:blank
          (dns-domain? longest-valid-label)
          (dns-domain? longest-valid-domain)
          (dns-domain? (string-append longest-valid-label "a"))
          (dns-domain? (string-append longest-valid-domain "a"))]
}

@defproc[(valid-url-string? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a “valid URL” for use in feeds. For this library’s purposes, a
valid URL is one which, when parsed with @racket[string->url], includes a valid @tt{scheme} part
(e.g. @racket{http://}), and in which the host is a @racket[dns-domain?] (and not, say, an IP
address).

@examples[#:eval mod-constructs
          (valid-url-string? "http://rclib.example.com")
          (valid-url-string? "telnet://rclib.example.com")
          (code:line (valid-url-string? "gonzo://example.com") (code:comment @#,elem{scheme need not be registered}))
          (code:line (valid-url-string? "https://user:p@example.com:8080") (code:comment @#,elem{includes user/password/port}))
          (code:line (valid-url-string? "file://C:\\home\\user?q=me") (code:comment @#,elem{Look, you do you}))
          code:blank
          (code:comment @#,elem{Valid URIs but not URLs:})
          (code:line (valid-url-string? "news:comp.servers.unix") (code:comment @#,elem{no host given, only path}))
          (code:line (valid-url-string? "http://subdomain-.example.com") (code:comment @#,elem{invalid label}))
          code:blank
          (code:line (code:comment @#,elem{Valid URLs but not allowed by this library for use in feeds}))
          (code:line (valid-url-string? "ldap://[2001:db8::7]/c=GB?objectClass?one") (code:comment @#,elem{Host is not a DNS domain}))
          (code:line (valid-url-string? "telnet://192.0.2.16:80/") (code:comment @#,elem{ditto}))]

}

@defproc[(email-address? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a valid email address according to what is essentially a
common-sense subset of RFC 5322:

@itemlist[

@item{Must be in the format @racketvalfont{@nonterm{local-part}@litchar{@"@"}@nonterm{domain}}}

@item{The @nonterm{local-part} must be no longer than 65 bytes and only include @litchar{a–z},
@litchar{A–Z}, @litchar{0–9}, or characters in the set @litchar|{!#$%&'*+/=?^_‘{|}~-.}|.}

@item{The @nonterm{domain} must be valid according to @racket[dns-domain?].}

@item{The entire email address must be no longer than 255 bytes.}

]

@examples[#:eval mod-constructs
          (email-address? "test-email.with+symbol@example.com")
          (email-address? "#!$%&'*+-/=?^_{}|~@example.com")
          code:blank
          (code:comment @#,elem{See also dns-domain? which applies to everything after the @"@" sign})
          (email-address? "email@123.123.123.123")
          (email-address? "λ@example.com")]

}

@defproc[(validate-email-address [addr string?]) boolean?]{

Returns @racket[_addr] if it is a valid email address (according to the same rules as for
@racket[email-address?]); otherwise, an exception is raised whose message explains the reason the
address is invalid.

@interaction[#:eval mod-constructs
             (validate-email-address "marian@rclib.example.com")
             (validate-email-address "@")
             (validate-email-address "me@myself@example.com")
             (validate-email-address ".marian@rclib.example.com")
             (validate-email-address "λ@example.com")
             (validate-email-address "lambda@1.example.com")]

}

@section{Language codes}

@defthing[system-language (promise/c iso-639-language-code?)]{

A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{promise} that, when @racket[force]d,
yields a two-letter symbol corresponding to the default language in use for the current user
account/system. On Unix and Mac OS, the first two characters of the value returned by
@racket[system-language+country] are used. On Windows, the first two characters of the value in the
registry key @tt{HKEY_CURRENT_USER\Control Panel\International\LocaleName} are used. If the system
language cannot be determined, an exception is raised the first time the promise is forced.

@examples[#:eval mod-constructs
          (force system-language)]

}

@defproc[(iso-639-language-code? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a two-character lowercase symbol matching a two-letter
@hyperlink["https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes"]{ISO639-1 language code}.

@examples[#:eval mod-constructs
          (iso-639-language-code? 'fr)
          (iso-639-language-code? 'FR)]

}

@defthing[language-codes (listof iso-639-language-code?)]{

A list of symbols that qualify as @racket[iso-639-language-code?].

}
