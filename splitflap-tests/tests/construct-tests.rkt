#lang racket/base

;; Unit tests for splitflap/constructs

(require gregor
         racket/file
         racket/runtime-path
         rackunit
         splitflap/private/validation
         splitflap/private/feed)

;; ~~ DNS Domain validation (RFC 1035) ~~~~~~~~~~~

(define longest-valid-label (make-string 62 #\a))
(define longest-valid-domain
  (string-append longest-valid-label ; 63 bytes (including length header)
                 "." longest-valid-label ; 126
                 "." longest-valid-label ; 189
                 "." longest-valid-label ; 252
                 ".aa"))               ; 255 bytes
  
(check-true (dns-domain? "example.com"))
(check-true (dns-domain? "example.com"))
(check-true (dns-domain? "ex-ample.com"))
(check-true (dns-domain? "EXAMPLE.COM"))
(check-true (dns-domain? "a12-345.b6-78"))
(check-true (dns-domain? "a"))
(check-true (dns-domain? "a.b.c.d.e.f.g.h"))
(check-true (dns-domain? longest-valid-label))
(check-true (dns-domain? (string-append longest-valid-label ".com")))
(check-true (dns-domain? longest-valid-domain))
  
(check-false (dns-domain? " example.com")) ; leading space
(check-false (dns-domain? "example.com ")) ; trailing space
(check-false (dns-domain? "ex ample.com")) ; internal space
(check-false (dns-domain? "example-.com")) ; label ending in hyphen
(check-false (dns-domain? "example.com-")) ; another
(check-false (dns-domain? "12345.b"))        ; label starting with number
(check-false (dns-domain? "a12345.678"))     ; another
(check-false (dns-domain? (string-append longest-valid-label "a")))
(check-false (dns-domain? (string-append longest-valid-domain "a")))

;; ~~ Email address validation (subset of RFC5322) ~~~~~~~~~

(check-true (email-address? "test@domain.com"))
(check-true (email-address? "test-email.with+symbol@domain.com"))
(check-true (email-address? "id-with-dash@domain.com"))
(check-true (email-address? "_______@example.com"))
(check-true (email-address? "#!$%&'*+-/=?^_{}|~@domain.org"))
(check-true (email-address? "\"email\"@example.com"))

;; See also the tests for dns-domain? which apply to everything after the @
(check-false (email-address? "email"))
(check-false (email-address? "email@"))
(check-false (email-address? "@domain.com"))
(check-false (email-address? "@"))
(check-false (email-address? ""))
(check-false (email-address? (string-append "test@" longest-valid-domain)))
(check-false (email-address? "email@123.123.123.123"))
(check-false (email-address? "email@[123.123.123.123]"))

;; ~~ URL Validation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(check-true (valid-url-string? "https://example.com"))
(check-true (valid-url-string? "ftp://example.com"))          ; FTP scheme
(check-true (valid-url-string? "gonzo://example.com"))        ; scheme need not be registered
(check-true (valid-url-string? "https://user:p@example.com")) ; includes user/password
(check-true (valid-url-string? "https://example.com:8080"))   ; includes port
(check-true (valid-url-string? "file://C:\\home\\user?q=me"))   ; OK whatever

;; Things that are valid URIs but not valid URLs
(check-false (valid-url-string? "news:comp.servers.unix")) ; no host given, only path
(check-false (valid-url-string? "http://ex ample.com"))  ; domain not RFC 1035 compliant

;; Things that are actually valid URLs but I say nuh-uh, not for using in feeds
(check-false (valid-url-string? "ldap://[2001:db8::7]/c=GB?objectClass?one"))
(check-false (valid-url-string? "telnet://192.0.2.16:80/"))

;; ~~ Tag URIs (RFC 4151) ~~~~~~~~~~~~~~~~~~~~~~~~
(check-true (tag-specific-string? "abcdefghijklmnopqrstuvwxyz0123456789"))
(check-true (tag-specific-string? "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(check-true (tag-specific-string? "_.~,;=$&'@"))
(check-true (tag-specific-string? "!()*+:/-"))
(check-true (tag-specific-string? ""))
(check-false (tag-specific-string? "a\\"))
(check-false (tag-specific-string? "a "))
(check-false (tag-specific-string? "aå"))
(check-false (tag-specific-string? "a^"))

(define invalid-specific " tra^^shy\\` GarB§ºage ###")
(check-false (tag-specific-string? invalid-specific))
(check-true (tag-specific-string? (normalize-tag-specific invalid-specific)))

;; RFC 4151 section 2.4 — Equality of tags:
;; “Tags are simply strings of characters and are considered equal if and
;;  only if they are completely indistinguishable in their machine
;;  representations when using the same character encoding.  That is, one
;;  can compare tags for equality by comparing the numeric codes of their
;;  characters, in sequence, for numeric equality.  This criterion for
;;  equality allows for simplification of tag-handling software, which
;;  does not have to transform tags in any way to compare them.”
(check-true (tag=? (mint-tag-uri "example.com" "2005" "main")
                   (mint-tag-uri "example.com" "2005" "main")))
;; Comparison is case-sensitive?
(check-false (tag=? (mint-tag-uri "Example.com" "2005" "main")
                    (mint-tag-uri "example.com" "2005" "main")))
;; Date equivalency doesn’t count
(check-false (tag=? (mint-tag-uri "Example.com" "2005-01" "main")
                    (mint-tag-uri "Example.com" "2005-01-01" "main")))


;; ~~ Dates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(check-true (moment<=? (now/moment) (infer-moment)))
(check-equal? (infer-moment "2022-04-08")
              (moment 2022 4 8))
(check-exn exn:fail:contract? (lambda () (infer-moment "2022")))


;; ~~ Persons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define joel (person "Joel" "joel@example.com"))
(check-true (person? joel))
(check-equal? (person->xexpr joel 'author 'rss) '(author "joel@example.com (Joel)"))
(check-equal? (person->xexpr joel 'author 'atom) '(author (name "Joel") (email "joel@example.com")))

;; Prefixing child elements
(check-equal? (person->xexpr joel 'itunes:owner 'itunes)
              '(itunes:owner (itunes:name "Joel") (itunes:email "joel@example.com")))



;; ~~ MIME types ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Check some common types
(check-equal? (path/string->mime-type ".mp3") "audio/mpeg")
(check-equal? (path/string->mime-type ".m4a") "audio/mp4")
(check-equal? (path/string->mime-type ".mpg") "video/mpeg")
(check-equal? (path/string->mime-type ".mp4") "video/mp4")

;; Can use paths
(check-equal? (path/string->mime-type (build-path "." "file.epub")) "application/epub+zip")

;; Empty list returned for unknown extensions
(check-equal? (path/string->mime-type ".asdahsf") #f)



;; ~~ Enclosures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define test-enc
  (enclosure "gopher://example.com/greeting.m4a" "audio/mp4" 1234))

(check-equal?
 (express-xml test-enc 'atom #:as 'xexpr)
 '(link [[rel "enclosure"]
         [href "gopher://example.com/greeting.m4a"]
         [length "1234"]
         [type "audio/mp4"]]))
  
(check-equal?
 (express-xml test-enc 'rss #:as 'xexpr)
 '(enclosure [[url "gopher://example.com/greeting.m4a"]
              [length "1234"]
              [type "audio/mp4"]]))

;; Enclosure with unknown type
(define test-enc2
  (enclosure "gopher://example.com/greeting.m4a" #f 1234))
  
(check-equal?
 (express-xml test-enc2 'atom #:as 'xexpr)
 '(link [[rel "enclosure"]
         [href "gopher://example.com/greeting.m4a"]
         [length "1234"]]))
  
(check-equal?
 (express-xml test-enc2 'rss  #:as 'xexpr)
 '(enclosure [[url "gopher://example.com/greeting.m4a"]
              [length "1234"]]))

(define-runtime-path temp "temp.mp3")
(display-to-file (make-bytes 100 65) temp #:exists 'truncate)
(check-equal?
 (express-xml (file->enclosure temp "http://example.com") 'atom #:as 'xexpr)
 '(link [[rel "enclosure"]
         [href "http://example.com/temp.mp3"]
         [length "100"]
         [type "audio/mpeg"]]))
(delete-file temp)