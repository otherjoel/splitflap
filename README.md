# racket-rss

🔖⚛️ RSS / Atom feed generation library for Racket. **Still in progress**

Purposes (in order of importance and implementation):

 * Convenient generating of valid, best-practice Atom 1.0 and RSS 2.0 feeds suitable for blogging. 
 * Same, but for podcast feeds, following [Apple’s podcast feed requirements][1])
 * Parsing RSS feeds
  
You should only have to supply what is necessary to generate the elements required by the spec.

Everything you supply is validated, so the result is always either a valid feed or an exception.

The library will actually be more opinionated than the spec about how your feed is constructed. This
supports the convenience aspect because you have fewer decisions to make, and makes it easier to
guarantee a valid feed.

## Useful links

* [Atom Spec (RFC 4287)](https://datatracker.ietf.org/doc/html/rfc4287)
* ['Tag' URI Scheme spec (RFC 4151)][tag] for minting feed IDs
* [What’s the difference between a URI and a URL?][udiff]

[tag]: https://datatracker.ietf.org/doc/html/rfc4151
[udiff]: https://danielmiessler.com/study/difference-between-uri-url/

[1]: https://podcasters.apple.com/support/823-podcast-requirements
