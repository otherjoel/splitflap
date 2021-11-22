#lang scribble/manual

@(require "misc.rkt" (for-label splitflap xml))

@title{Package Notes}

@section{Known Issues}

See Splitflap’s @hyperlink["https://github.com/otherjoel/splitflap/issues"]{issues tracker on Github} for any
known problems with the library, or to report problems.

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
