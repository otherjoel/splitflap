#lang scribble/manual

@(require
   scribble/examples
   (for-label splitflap racket/base))

@title{Quick start}

There are four simple steps to building a feed with this library:

@(define tutorial (make-base-eval #:lang 'racket/base))
@(tutorial '(require splitflap))

@section{Step 1: Mint a tag URI}

Every feed needs a globally unique identifier, and this library requires you to use @tech{tag URIs}
for this purpose. To mint a tag URI, you provide three things: a domain (or an email address); a
date, and a specific identifier:

@(examples
  #:eval tutorial
  #:label #false
  (define my-id (mint-tag-uri "my-domain.com" "2012" "blog")))

The idiomatic route is to create a tag URI for the entire feed, and then append to that URI to
create tag URIs for the individual items in that feed.

See @tech{Tag URIs} for more information.

@section{Step 2: Create a list of items}

You’ll generally want to write a function that converts your item data from its original format into
@racket[feed-item]s, and then create your list by mapping that function over each of the items.

For this tutorial, we’ll manually make a list with just one @racket[feed-item] in it:

@(examples
  #:eval tutorial
  #:label #false
  (define my-items
    (list
     (feed-item
      (append-specific my-id "first-post")       (code:comment @#,elem{item-specific ID})
      "https://my-domain.com/first-post.html"    (code:comment @#,elem{URL})
      "Chaucer, Rabelais and Balzac"             (code:comment @#,elem{title})
      (person "Marian Paroo" "marian@rclib.org") (code:comment @#,elem{author})
      (infer-moment "1912-06-21")                (code:comment @#,elem{publish date})
      (infer-moment "1912-06-21")                (code:comment @#,elem{updated date})
      '(article (p "My first post; content TK"))))))


@section{Step 3: Create your feed}

The @racket[feed] struct combines all the elements we’ve created so far:

@(examples
  #:eval tutorial
  #:label #false
  (define my-feed
    (feed
     my-id                     (code:comment @#,elem{tag URI})
     "http://rclib.org/blog"   (code:comment @#,elem{site URL})
     "River City Library Blog" (code:comment @#,elem{Title})
     my-items)))

@section{Step 4: Generate the XML for your feed}

Final step: pass your feed to @racket[express-xml], specifying either the @racket['atom] or
@racket['rss] format, and provide a URL where the feed itself will be accessible:

@(examples
  #:eval tutorial
  #:label #false
  (display (express-xml my-feed 'atom "https://rclib.org/feed.atom")))

There you go! Save that string in a file and you’ve got yourself a valid Atom 1.0 feed.

Let’s do one in RSS format, for kicks. Note the different URL for this version of the feed:

@(examples
  #:eval tutorial
  #:label #false
  (display (express-xml my-feed 'rss "https://rclib.org/feed.rss")))

If you want the result as an X-expression, you can do that too, using the @racket[#:as] keyword
argument:

@(examples
  #:eval tutorial
  #:label #false
  (express-xml my-feed 'rss "https://rclib.org/feed.rss" #:as 'xexpr))

@section{Wrap up}

Now you know how to use this library to create a feed for your website.

To create a podcast feed, just use @racket[episode] instead of @racket[feed-item], and
@racket[podcast] instead of @racket[feed]. Check out the module reference for details of using those
functions.
