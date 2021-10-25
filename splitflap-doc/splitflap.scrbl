#lang scribble/manual

@(require "misc.rkt"
          scribble/core
          scribble/html-properties)

@title[#:style 'toc]{Splitflap: Atom and RSS Feeds}
@author[(author+email "Joel Dueck" "joel@jdueck.net")]

@flipboard-div
@flipboard-script

@defmodule[splitflap]

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

@bold{Splitflap is in beta release.} The interface is mostly stable and documentation is fairly
complete. There are some wrinkles to iron out. Please report any problems on
@hyperlink["https://github.com/otherjoel/splitflap/issues"]{the Github repo}.

@bold{Installation:} To install this package from the command line:

@terminal{
 @:>{raco pkg install splitflap}}

Or using DrRacket: click the @onscreen{File} menu → @onscreen{Install Package …}.

@bold{Source and license:} The source code for this library is
@hyperlink["https://github.com/otherjoel/splitflap"]{on GitHub}, and is provided under the terms
of the @hyperlink["https://github.com/otherjoel/splitflap/blob/main/LICENSE.md"]{Blue Oak 1.0.0
license}.

@local-table-of-contents[]

@include-section["tutorial.scrbl"]
@include-section["mod-splitflap.scrbl"]
@include-section["constructs.scrbl"]
@include-section["notes.scrbl"]
