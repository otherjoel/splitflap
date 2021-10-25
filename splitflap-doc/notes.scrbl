#lang scribble/manual

@(require "misc.rkt" (for-label splitflap xml))

@title{Package Notes}

@section{Traps for the unwary}

@subsection{Defeating validation}

A design goal of Splitflap is that if the data you supply would result in an invalid feed, you get
an exception. There are some exceptions to this, however. Below is a list of the ones I know about.
Most of these should/will be fixed at some point, but I haven’t yet decided on the right approach.

@itemlist[

@item{The main content of a feed item could contain the forbidden string @litchar{]]>} which would
prematurely end the CDATA block.}

@item{If you include HTML tags within any string content other than the main content (such as a
title, person’s name, etc.), whether escaped or not, the resulting feed is likely to be invalid.}

@item{When creating @racket[enclosure]s manually, it is possible to give the enclosure an invalid
MIME type, or a MIME type that doesn’t match the file.}

]

You should run all your feeds through the @W3CFeedValidator[]. Please
@hyperlink["https://github.com/otherjoel/splitflap/issues"]{let me know} of any failures you
encounter when checking feeds created with Splitflap.

@subsection{Fiddling with X-expressions}

The @racket[express-xml] function offers the option of giving you a feed in the form of a tagged X-expression,
in case you want to fiddle with it first.

Splitflap does not currently expose its customized entity escaping functionality, which correctly
handles characters required to be escaped by @AppleRequirements[] but which are not escaped by the
XML library’s @racket[xexpr->string]. Keep this in mind if you are making custom modifications to
the X-expressions returned by @racket[express-xml] when called with @racket[#:as 'xexpr] (which are
returned without any entity escaping.

@subsection{Non-ASCII email addresses and domains}

The @Atom1.0[] and @RSS2.0[] specs do not contemplate or allow for the use of non-ASCII characters
in email addresses or domain names, which is a defect considering that non-English alphabets are in
widespread use for both of these things.

At this early stage I have chosen to enforce the standards as written, for consistency’s sake. I do
plan to add a parameter which would cause @racket[dns-domain?] and @racket[email-address?] to
validate strings according to some alternative scheme that allows for non-ASCII characters. In order
to do this correctly, though, I need to educate myself about any standards that exist in this area.

@section{Licensing}

Splitflap is provided under the terms of the
@hyperlink["https://github.com/otherjoel/splitflap/blob/main/LICENSE.md"]{Blue Oak 1.0.0 license}.

The split-flap animation in the HTML edition of this documentation comes from the 
@hyperlink["https://github.com/rjkerrison/ticker-board"]{ticker-board} project by
Robin James Kerrison, under the terms of the
@hyperlink["https://github.com/otherjoel/splitflap/blob/main/NOTICE.md"]{MIT license.}
