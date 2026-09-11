#lang racket/base

;; Unit tests for splitflap/constructs

(require gregor
         racket/file
         racket/runtime-path
         rackunit
         splitflap)

;; ~~ DNS Domain validation (RFC 1035) ~~~~~~~~~~~

(define longest-valid-label (make-string 63 #\a))
(define longest-valid-domain
  (string-append longest-valid-label
                 "." longest-valid-label
                 "." longest-valid-label
                 "." (make-string 61 #\a)))
(check-equal? (string-length longest-valid-domain) 253)

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
(check-true (dns-domain? "12345.b"))
(check-true (dns-domain? "a.1.b"))

(check-false (dns-domain? ""))
(check-false (dns-domain? "."))
(check-false (dns-domain? "example..com"))
(check-false (dns-domain? " example.com")) ; leading space
(check-false (dns-domain? "example.com ")) ; trailing space
(check-false (dns-domain? "ex ample.com")) ; internal space
(check-false (dns-domain? "example-.com")) ; label ending in hyphen
(check-false (dns-domain? "example.com-")) ; another
(check-false (dns-domain? "-example.com"))
(check-false (dns-domain? "a12345.678"))
(check-false (dns-domain? "192.0.2.16"))
(check-false (dns-domain? (string-append longest-valid-label "a")))
(check-false (dns-domain? (string-append longest-valid-domain "a")))

;; ~~ Email address validation (subset of RFC5322) ~~~~~~~~~

(check-true (email-address? "test@domain.com"))
(check-true (email-address? "test-email.with+symbol@domain.com"))
(check-true (email-address? "id-with-dash@domain.com"))
(check-true (email-address? "_______@example.com"))
(check-true (email-address? "#!$%&'*+-/=?^_{}|~@domain.org"))
(check-true (email-address? "a`b@example.com"))
(check-true (email-address? "ABC@example.com"))
(check-true (email-address? "test@1.example.com"))
(check-true (email-address? (string-append (make-string 64 #\a) "@example.com")))

;; See also the tests for dns-domain? which apply to everything after the @
(check-false (email-address? "email"))
(check-false (email-address? "email@"))
(check-false (email-address? "@domain.com"))
(check-false (email-address? "@"))
(check-false (email-address? ""))
(check-false (email-address? (string-append "test@" longest-valid-domain)))
(check-false (email-address? "email@123.123.123.123"))
(check-false (email-address? "email@[123.123.123.123]"))
(check-false (email-address? "\"email\"@example.com"))
(check-false (email-address? "a b@example.com"))
(check-false (email-address? "aλ@example.com"))
(check-false (email-address? "a‘b@example.com"))
(check-false (email-address? ".a@example.com"))
(check-false (email-address? "a.@example.com"))
(check-false (email-address? "a..b@example.com"))
(check-false (email-address? "a@b@example.com"))
(check-false (email-address? (string-append (make-string 65 #\a) "@example.com")))

(for ([addr (in-list '("Joel@example.com" "a`b@example.com" "a..b@example.com" "a‘b@example.com"
                       "a b@example.com" "a@b@example.com" "@example.com" "email@123.123.123.123"))])
  (check-equal? (with-handlers ([exn:fail:contract? (λ (e) #f)])
                  (equal? addr (validate-email-address addr)))
                (email-address? addr)
                addr))

;; ~~ URL Validation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(check-true (valid-url-string? "https://example.com"))
(check-true (valid-url-string? "ftp://example.com"))          ; FTP scheme
(check-true (valid-url-string? "gonzo://example.com"))        ; scheme need not be registered
(check-true (valid-url-string? "https://user:p@example.com")) ; includes user/password
(check-true (valid-url-string? "https://example.com:8080"))   ; includes port
(check-true (valid-url-string? "file://C:\\home\\user?q=me"))   ; OK whatever
(check-true (valid-url-string? "https://1.example.com"))

;; Things that are valid URIs but not valid URLs
(check-false (valid-url-string? "news:comp.servers.unix")) ; no host given, only path
(check-false (valid-url-string? "http://ex ample.com"))  ; domain not RFC 1035 compliant
(check-false (valid-url-string? "https://"))
(check-false (valid-url-string? "https:///path"))

;; Things that are actually valid URLs but I say nuh-uh, not for using in feeds
(check-false (valid-url-string? "ldap://[2001:db8::7]/c=GB?objectClass?one"))
(check-false (valid-url-string? "telnet://192.0.2.16:80/"))

;; ~~ URL convenience functions ~~~~~~~~~~~~~~~~~~

(check-equal? (url-domain "http://example.com") "example.com")
(check-equal? (url-domain "https://user:p@example.com:8080/path/to/file") "example.com")
(check-exn exn:fail:contract? (lambda () (url-domain "x")))

;; ensure paths are tested accurately on all platforms
(define rel-path (build-path "path" "to" "my file.html"))
(check-true (relative-path? rel-path))
(define abs-path (path->complete-path rel-path))
(check-true (absolute-path? abs-path))

(check-equal? (url-join "http://example.com" rel-path)            ; bare domain w/o trailing slash
              "http://example.com/path/to/my%20file.html")
(check-equal? (url-join "http://example.com/" rel-path)           ; bare domain w/trailing slash
              "http://example.com/path/to/my%20file.html")
(check-equal? (url-join "http://example.com/dir/final" rel-path)  ; final elem w/o trailing slash removed
              "http://example.com/dir/path/to/my%20file.html")     
(check-equal? (url-join "http://example.com/my dir/" rel-path) ; final element w/trailing slash kept
              "http://example.com/my%20dir/path/to/my%20file.html")
(check-exn exn:fail:contract? (lambda () (url-join "x" "x"))) ; invalid URL string
(check-exn exn:fail:contract? (lambda () (url-join "http://example.com" abs-path))) ; no absolute paths

;; ~~ Internationalized domain names ~~~~~~~~~~~~~

(check-equal? (domain->ascii "bücher.example") "xn--bcher-kva.example")
(check-equal? (domain->ascii "faß.de") "xn--fa-hia.de")
(check-equal? (domain->ascii "例え.テスト") "xn--r8jz45g.xn--zckzah")
(check-equal? (domain->ascii "مثال.إختبار") "xn--mgbh0fb.xn--kgbechtv")
(check-equal? (domain->ascii "EXAMPLE.com") "EXAMPLE.com")
(check-equal? (domain->ascii "XN--BCHER-KVA.example") "XN--BCHER-KVA.example")

(for ([valid (in-list '("\u0915\u094D\u200D\u0937.example"
                        "\u0646\u0627\u0645\u0647\u200C\u0627\u06CC.example"
                        "col\u00B7legi.example"
                        "\u30A2\u30FB\u30A4.example"
                        "\u03B1\u0375\u03B2.example"
                        "\u05D0\u05F3.example"))])
  (check-not-exn (λ () (domain->ascii valid)) valid))

(for ([invalid (in-list '(""
                          "bücher..example"
                          "Bücher.example"
                          "bu\u0308cher.example"
                          "-bücher.example"
                          "bü--cher.example"
                          "\u0301bücher.example"
                          "a\u200Cb.example"
                          "a\u00B7b.example"
                          "a\u30FBb.example"
                          "\u0660\u06F0.example"
                          "\U10E80.example"
                          "مثال.1example"
                          "xn--zzzzzzzz.example"
                          "ab--cd.example"))])
  (check-exn exn:fail:contract? (λ () (domain->ascii invalid)) invalid))

(check-exn #rx"suggestion: \"bücher.example\"" (λ () (domain->ascii "Bücher.example")))
(check-exn #rx"suggestion: \"例え.テスト\"" (λ () (domain->ascii "例え。テスト")))

(check-true (dns-domain? "xn--bcher-kva.example"))
(check-false (dns-domain? "bücher.example"))
(check-false (dns-domain? "xn--zzzzzzzz.example"))
(check-false (dns-domain? "ab--cd.example"))
(check-false (dns-domain? "xn--mgbh0fb.1example"))

(check-equal? (url-string->ascii "https://bücher.example/straße?q=ü#ß")
              "https://xn--bcher-kva.example/stra%C3%9Fe?q=%C3%BC#%C3%9F")
(check-equal? (url-string->ascii "https://user:pä@bücher.example:8080/")
              "https://user:p%C3%A4@xn--bcher-kva.example:8080/")
(for ([url (in-list '("https://example.com/?q=a+b&c=%20"
                      "file://C:\\home\\user?q=me"
                      "https://user:p@example.com:8080"))])
  (check-equal? (url-string->ascii url) url))
(check-exn exn:fail:contract? (λ () (url-string->ascii "news:comp.servers.unix")))
(check-exn exn:fail:contract? (λ () (url-string->ascii "https://Bücher.example/")))
(check-false (valid-url-string? "https://example.com/café"))
(check-false (valid-url-string? "https://bücher.example/"))

(check-equal? (email-address->ascii "marian@bücher.example") "marian@xn--bcher-kva.example")
(check-equal? (email-address->ascii "marian@example.com") "marian@example.com")
(check-exn exn:fail:contract? (λ () (email-address->ascii "marían@example.com")))
(check-exn exn:fail:contract? (λ () (email-address->ascii "marian@Bücher.example")))
(check-false (email-address? "marian@bücher.example"))

(check-equal? (tag-uri->string (mint-tag-uri (domain->ascii "bücher.example") "2024" "blog"))
              "tag:xn--bcher-kva.example,2024:blog")

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

(check-true (tag-authority? "example.com"))
(check-true (tag-authority? "1.example.com"))
(check-true (tag-authority? "kate.p_1-x@example.com"))
(check-false (tag-authority? ""))
(check-false (tag-authority? "Example.com"))
(check-false (tag-authority? "Kate@example.com"))
(check-false (tag-authority? "kate+feeds@example.com"))
(check-false (tag-authority? "kate@Example.com"))
(check-exn exn:fail:contract? (λ () (mint-tag-uri "Example.com" "2005" "main")))

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
(check-false (tag=? (mint-tag-uri "example.com" "2005" "Main")
                    (mint-tag-uri "example.com" "2005" "main")))
;; Date equivalency doesn’t count
(check-false (tag=? (mint-tag-uri "example.com" "2005-01" "main")
                    (mint-tag-uri "example.com" "2005-01-01" "main")))


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

(check-equal? (hash-ref mime-types-by-ext 'epub) "application/epub+zip")
(check-true (immutable? mime-types-by-ext))



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