#lang scribble/manual

@(require "misc.rkt")

@title[#:style 'toc]{Splitflap: Atom and RSS Feeds}
@author[(author+email "Joel Dueck" "joel@jdueck.net")]

@flipboard-div
@flipboard-script

@defmodule[splitflap #:use-sources (splitflap/private/feed)]

This library provides a simple interface for building valid Atom and RSS feeds, including podcast
feeds, for your web properties.

It’s not mechanically difficult to generate feeds without any special libraries, but there are a lot
of tedious details to get right.  Syndication feeds involve several layers of standards and
specifications about how different types data should be encoded, what elements are required, and so
forth. Adhering strictly to those standards is not only good citizenship, it’s the best way of
preventing problems for your subscribers.

With this library, you are only made to supply the minimum set of data needed to produce a feed.
But everything you supply is carefully validated, so that the result is either a fully valid feed or
an exception.

@callout{If you use Splitflap in your project, @hyperlink["mailto:joel@jdueck.net"]{email Joel} to
introduce yourself! This is the sole condition of the project’s
@hyperlink["https://github.com/otherjoel/splitflap/blob/main/LICENSE.md"]{permissive license.} (See
@hyperlink["https://joeldueck.com/how-i-license.html"]{How I License} for background.)}

@local-table-of-contents[]

@include-section["tutorial.scrbl"]
@include-section["mod-splitflap.scrbl"]
@include-section["constructs.scrbl"]
@include-section["notes.scrbl"]
