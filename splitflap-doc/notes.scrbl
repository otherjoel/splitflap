#lang scribble/manual

@(require "misc.rkt"
          splitflap/private/version
          (for-label splitflap xml racket/promise))

@title{Package Notes (@splitflap-version[])}

Splitflap can be considered stable. No backward-incompatible changes are planned.

@section{Known Issues}

See Splitflap’s @hyperlink["https://github.com/otherjoel/splitflap/issues"]{issues tracker on Github}
for any known problems with the library, or to report problems.

@subsection{Non-ASCII email addresses and domains}

The @Atom1.0[] and @RSS2.0[] specs do not contemplate or allow for the use of non-ASCII characters
in email addresses or domain names, which is a defect considering that non-English alphabets are in
widespread use for both of these things.

At this early stage I have chosen to enforce the standards as written, for consistency’s sake. I do
plan to add a parameter which would cause @racket[dns-domain?] and @racket[email-address?] to
validate strings according to some alternative scheme that allows for non-ASCII characters. In order
to do this correctly, though, I need to educate myself about any standards that exist in this area.

@section{Version History}

@subsection{Version 1.4}

@itemlist[#:style 'compact

@item{@racket[dns-domain?] now allows labels that start with a digit (per RFC 1123) and labels of up
to 63 bytes; limits domains to 253 bytes; and no longer accepts the empty string. As a result,
@racket[valid-url-string?] no longer accepts URLs with an empty host.}

@item{@racket[email-address?] now checks the entire local part (previously one allowed character was
enough), and it now applies exactly the same rules as @racket[validate-email-address]. Both accept
uppercase letters and @litchar{`}, and both reject local parts that start or end with a period or
contain two periods in a row.}

@item{Added @racket[tag-authority?]. @racket[mint-tag-uri] now requires a lowercase authority, and
allows only @litchar{a–z}, @litchar{0–9}, @litchar{-}, @litchar{.} and @litchar{_} before the
@litchar{@"@"} of an email authority, per RFC 4151. The @W3CFeedValidator[] rejects Atom feeds whose
tag URIs use other authorities.}

]

@subsection{Version 1.3}

@itemlist[#:style 'compact

@item{Made @racket[mime-types-by-ext] a plain hash table rather than a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{promise}. You no longer need to use
@racket[force] to access the table (though that will still work).}

]

@subsection{Version 1.2}

@itemlist[#:style 'compact

@item{Fix exception raised in @racket[express-xml] when no entries are present
(@link["https://github.com/otherjoel/splitflap/issues/9"]{#9})}

@item{Added zero-arity form of @racket[infer-moment] to get current moment}

]

@subsection{Version 1.1}

@itemlist[#:style 'compact

@item{Fix unquoting bug in @racket[person] x-expressions
(@link["https://github.com/otherjoel/splitflap/pull/7"]{#7})}

@item{Remove dependency on @racketmodname[txexpr] package
(@link["https://github.com/otherjoel/splitflap/pull/8"]{#8})}

@item{Ensure @racket[system-language] works with Racket CS 8.4+
(@link["https://github.com/otherjoel/splitflap/commit/0da67ccdc7c0e7f84c5a34cd88f627d65fbb86f4"]{@tt{0da67ccd}})}

]


@section{Licensing}

Splitflap is provided under the terms of the
@hyperlink["https://github.com/otherjoel/splitflap/blob/main/LICENSE.md"]{Blue Oak 1.0.0 license}.

The split-flap animation in the HTML edition of this documentation comes from the 
@hyperlink["https://github.com/rjkerrison/ticker-board"]{ticker-board} project by
Robin James Kerrison, under the terms of the
@hyperlink["https://github.com/otherjoel/splitflap/blob/main/NOTICE.md"]{MIT license.}
