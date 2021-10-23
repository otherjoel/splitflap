#lang scribble/manual

@(require "misc.rkt" (for-label splitflap xml))

@title{Package Notes}

@section{Traps for the unwary}

@subsection{Fiddling with X-expressions}

The @racket[express-xml] function offers the option of giving you a feed in the form of a tagged X-expression,
in case you want to fiddle with it first.

  Splitflap does not currently expose its customized entity escaping functionality, which correctly handles
characters required to be escaped by @AppleRequirements[] but which are not escaped by the XML
library’s @racket[xexpr->string]. Keep this in mind if you are making custom modifications to the
X-expressions returned by @racket[express-xml] when called with @racket[#:as 'xexpr] (which are
returned without any entity escaping.

@subsection{Non-ASCII email addresses and domains}

The @Atom1.0[] and @RSS2.0[] specs do not seem to contemplate or allow for the use of non-ASCII characters in
email addresses or domain names, which is a defect considering that non-English alphabets are in widespread use
for both of these things.

At this early stage I have chosen to enforce the standards as written, for consistency’s sake. I do plan
to add a parameter which would cause @racket[dns-domain?] and @racket[email-address?] to validate
strings according to some alternative scheme that allows for non-ASCII characters. In order to do this correctly,
though, I need to educate myself about any standards that exist in this area.


