#lang racket/base

;;
;; Predicates and constructors for Dates, Persons, Tag URIs, URLs, email addresses and enclosures

(require (for-syntax racket/base)
         "private/xml-generic.rkt"
         gregor
         net/url-string
         racket/contract
         racket/match
         racket/runtime-path
         racket/string
         txexpr
         web-server/private/mime-types)

;; Interfaces

(provide dns-domain?
         valid-url-string?
         tag-entity-date?
         tag-specific-string?
         email-address?
         validate-email-address
         rss-dialect?
         
         mint-tag-uri
         tag-uri?
         tag-uri->string
         append-specific
                  
         person
         (rename-out [human? person?])
         person->xexpr

         infer-moment
         moment->string

         (struct-out enclosure)
         file->enclosure)

;; (private) convenience macro
(define-syntax (define-explained-contract stx)
  (syntax-case stx ()
    [(_ (NAME VAL) EXPECTED TEST-EXPR)
     #'(define NAME
         (flat-contract-with-explanation
          (λ (VAL)
            (cond
              [TEST-EXPR]
              [else
               (λ (blame)
                 (raise-blame-error blame VAL '(expected: EXPECTED given: "~e") VAL))]))
          #:name 'NAME))]))

;; ~~ DNS Domain validation (RFC 1035) ~~~~~~~~~~~
;;
;;  <domain> ::= <subdomain> | " "
;;  <subdomain> ::= <label> | <subdomain> "." <label>
;;  <label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
;;  <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
;;  <let-dig-hyp> ::= <let-dig> | "-"
;;  <let-dig> ::= <letter> | <digit>
;;  <letter> ::= any one of the 52 alphabetic characters A through Z in
;;  upper case and a through z in lower case
;;  <digit> ::= any one of the ten digits 0 through 9
;;
;; The labels must follow the rules for ARPANET host names. They must
;; start with a letter, end with a letter or digit, and have as interior
;; characters only letters, digits, and hyphen.
;;
;; Per RFC 1035, each labels must be 63 characters or less including 1
;; byte for a length header in front of each label; and the total name length
;; must be 255 bytes or less including those header bytes.

(define-explained-contract (dns-domain? val)
  "valid RFC 1035 domain name"
  (and
   (string? val)
   (<
    (for/sum ([label (in-list (string-split val "." #:trim? #f))])
      (define label-length (bytes-length (string->bytes/utf-8 label)))
      (cond [(and (< label-length 63)
                  (regexp-match? #rx"^[a-zA-Z]([a-zA-Z0-9-]*[a-zA-Z0-9])?$" label))
             (+ 1 label-length)] ; Add one byte for length header per label
            [else 256])) ; use the length limit to disqualify the whole string if any one label is invalid
    256)))

(module+ test
  (require rackunit)

  (define longest-valid-label (make-string 62 #\a))
  (define longest-valid-domain
    (string-append longest-valid-label ; 63 bytes (including length header)
                   "." longest-valid-label ; 126
                   "." longest-valid-label ; 189
                   "." longest-valid-label ; 252
                   ".aa"))               ; 255 bytes
  
  (check-true (dns-domain? "joeldueck.com"))
  (check-true (dns-domain? "joeldueck.com"))
  (check-true (dns-domain? "joel-dueck.com"))
  (check-true (dns-domain? "ALLCAPS.COM"))
  (check-true (dns-domain? "a12-345.b6-78"))
  (check-true (dns-domain? "a"))
  (check-true (dns-domain? "a.b.c.d.e.f.g.h"))
  (check-true (dns-domain? longest-valid-label))
  (check-true (dns-domain? (string-append longest-valid-label ".com")))
  (check-true (dns-domain? longest-valid-domain))
  
  (check-false (dns-domain? " joeldueck.com")) ; leading space
  (check-false (dns-domain? "joeldueck.com ")) ; trailing space
  (check-false (dns-domain? "joel dueck.com")) ; internal space
  (check-false (dns-domain? "joeldueck-.com")) ; label ending in hyphen
  (check-false (dns-domain? "joeldueck.com-")) ; another
  (check-false (dns-domain? "12345.b"))        ; label starting with number
  (check-false (dns-domain? "a12345.678"))     ; another
  (check-false (dns-domain? (string-append longest-valid-label "a")))
  (check-false (dns-domain? (string-append longest-valid-domain "a"))))

;; ~~ URL Validation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; This library counts a URL as “valid” only if it includes a valid scheme
;; AND the host is a valid RFC 1035 domain.
(define-explained-contract (valid-url-string? val)
  "a URL that includes a valid scheme and a valid RFC 1035 domain as the host"
  (with-handlers ([url-exception? (lambda (e) #f)]) ; catch exns caused by “invalid scheme”
    (let ([u (string->url val)])
      (and (url-scheme u)
           (dns-domain? (url-host u))
           #t))))

(module+ test
  (check-true (valid-url-string? "https://joeldueck.com"))
  (check-true (valid-url-string? "ftp://joeldueck.com"))          ; FTP scheme
  (check-true (valid-url-string? "gonzo://joeldueck.com"))        ; scheme need not be registered
  (check-true (valid-url-string? "https://user:p@joeldueck.com")) ; includes user/password
  (check-true (valid-url-string? "https://joeldueck.com:8080"))   ; includes port
  (check-true (valid-url-string? "file://C:\\home\\user?q=me"))   ; OK whatever

  ;; Things that are valid URIs but not valid URLs
  (check-false (valid-url-string? "news:comp.servers.unix")) ; no host given, only path
  (check-false (valid-url-string? "http://joel dueck.com"))  ; domain not RFC 1035 compliant

  ;; Things that are actually valid URLs but I say nuh-uh, not for using in feeds
  (check-false (valid-url-string? "ldap://[2001:db8::7]/c=GB?objectClass?one"))
  (check-false (valid-url-string? "telnet://192.0.2.16:80/")))

;; ~~ Tag URIs (RFC 4151) ~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; The general syntax of a tag URI, in ABNF [2], is:
;;
;;      tagURI = "tag:" taggingEntity ":" specific [ "#" fragment ]
;;
;;   Where:
;;
;;      taggingEntity = authorityName "," date
;;      authorityName = DNSname / emailAddress
;;      date = year ["-" month ["-" day]]
;;      year = 4DIGIT
;;      month = 2DIGIT
;;      day = 2DIGIT
;;      DNSname = DNScomp *( "."  DNScomp ) ; see RFC 1035 [3]
;;      DNScomp = alphaNum [*(alphaNum /"-") alphaNum]
;;      emailAddress = 1*(alphaNum /"-"/"."/"_") "@" DNSname
;;      alphaNum = DIGIT / ALPHA
;;      specific = *( pchar / "/" / "?" ) ; pchar from RFC 3986 [1]
;;      fragment = *( pchar / "/" / "?" ) ; same as RFC 3986 [1]
;;
;; RFC 3986:
;;  pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
;;  unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
;;  sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
;;                   / "*" / "+" / "," / ";" / "="
;;
;; RFC 2234:
;;  ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z
;;  DIGIT          =  %x30-39             ; 0-9

;; Validates the portion labeled “specific” in the RFC 4151 grammar
(define-explained-contract (tag-specific-string? val)
  "a string containing only a–z, A–Z, 0–9 or chars in the set -._~~!$&'()*+,;=:@/?"
  (and (string? val)
       (regexp-match? #rx"^[a-zA-Z0-9_.~\\-\\!$&'\\(\\)\\*\\+,;=\\:@\\?/]*$" val)))

(define-explained-contract (tag-entity-date? val)
  "an RFC 4151 date string in the format YYYY[-MM[-DD]]"
  (and (string? val)
       (regexp-match? #px"^[0-9]{4}(-(0[1-9]|1[012])(-(0[1-9]|[12][0-9]|3[01]))?)?$" val)))

(struct tag-uri (authority date specific)
  #:methods gen:custom-write
  [(define (write-proc t port mode)
     (if mode (write-string "#<tag-uri " port) (write-string "(tag-uri " port))
     ((case mode
        [(#t) write]
        [(#f) display]
        [else (lambda (p port) (print p port mode))])
      (tag-uri->string t) port)
     (if mode (write-string ">" port) (write-string ")" port)))])

(define/contract (mint-tag-uri authority date specific)
  (-> dns-domain? tag-entity-date? tag-specific-string? tag-uri?)
  (tag-uri authority date specific))

(define/contract (append-specific t suffix)
  (-> tag-uri? tag-specific-string? tag-uri?)
  (struct-copy tag-uri t [specific (string-append (tag-uri-specific t) "." suffix)]))

(define/contract (tag-uri->string t #:specific [specific (tag-uri-specific t)])
  (->* (tag-uri?) (#:specific tag-specific-string?) string?)
  (format "tag:~a,~a:~a" (tag-uri-authority t) (tag-uri-date t) specific))

;; ~~ Email address validation (subset of RFC5322) ~~~~~~~~~

(define-explained-contract (email-address? str)
  "a valid RFC 5322 email address"      
  (and (string? str)
       (< (string-length str) 255) ; SMTP can handle a max of 254 characters
       (let-values
           ([(local-part domain)
             (match (string-split str "@")
               [(list loc dom) (values loc dom)]
               [_ (values #f #f)])])
         (and local-part
              (< (string-length local-part) 65)
              (dns-domain? domain)
              (regexp-match? #px"[a-z0-9!#$%&'*+/=?^_‘{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_‘{|}~-]+)*" local-part)))))

(define (email-error noun has-problem bad str)
  (raise-arguments-error 'validate-email-address (format "~a ~a" noun has-problem) noun bad "in" str))

(define (validate-email-address str)
  (unless (string? str) (raise-argument-error 'validate-email "string" str))
  (unless (< (string-length str) 255)
    (raise-argument-error 'validate-email "string no more than 254 bytes in length" str))
  (unless (string-contains? str "@") (email-error "address" "must contain @ sign" str str))
  (unless (not (regexp-match? #rx"@[^@]*@" str)) (email-error "address" "must not contain more than one @ sign" str str))
  (unless (not (regexp-match? #rx"@$" str)) (email-error "domain" "is missing" "" str))
  (unless (not (regexp-match? #rx"^@" str)) (email-error "local-part" "is missing" "" str))

  (match-define (list local-part domain) (string-split str "@"))
  (unless (< (string-length local-part) 65)
    (email-error "local part" "must be no longer than 64 bytes" local-part str))
  (unless (dns-domain? domain)
    (email-error "domain" "must be a valid RFC 1035 domain name" domain str))
  (unless (not (regexp-match? #rx"^\\." local-part))
    (email-error "local part" "must not start with a period" local-part str))
  (unless (regexp-match #rx"^[\\.a-z0-9!#$%&'*+/=?^_‘{|}~-]+$" local-part)
    (email-error "local part" "may only include a–z, A–Z, 0–9, or !#$%&'*+/=?^_‘{|}~-."
                 local-part
                 str))
  str)

;; Here’s a nearly-complete Regex for RFC5322. Not using it because it allows
;; for things that many/most email services & clients don’t actually support.
;; #px"^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\\[\\]-\x7f]|\\\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\\])$" str)))

(module+ test
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
  (check-false (email-address? "email@[123.123.123.123]")))

;; ~~ Dates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (n: v) (cond [(not v) 0] [(string? v) (string->number v)] [else v]))

(define/contract (infer-moment str)
  (-> string? moment?)
  (define date/time-regex
    #px"^([0-9]+)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])(?:\\s+([01]?[0-9]|2[0-3]):([0-5][0-9])(?::([0-5][0-9]|60))?)?")
  (match str
    [(pregexp date/time-regex (list _ y m d hr min sec))
     (moment (n: y) (n: m) (n: d) (n: hr) (n: min) (n: sec) 0)]
    [_ (raise-argument-error 'Date "string in the format ‘YYYY-MM-DD [hh:mm[:ss]]’" str)]))

(define/contract (moment->string m type)
  (-> moment? (or/c 'rss 'atom) string?)
  (~t m (case type
          [(atom) "y-MM-dd'T'HH:mm:ssXXXXX"]
          [(rss) "E, d MMM y HH:mm:ss xx"])))

;; ~~ Flavors of RSS: 'rss or 'atom ~~~~~~~~~~~~~~
(define-explained-contract (rss-dialect? v)
  "A symbol representing a valid RSS dialect: 'rss or 'atom"
  (or (eq? v 'rss) (eq? v 'atom)))

;; ~~ Persons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(struct human (name email uri))

(define/contract (person name email [uri #f])
  (->* (string? email-address?) ((or/c valid-url-string? #f)) human?)
  (human name email uri))
  
(define/contract (person->xexpr p entity dialect #:elem-prefix [prefix #f])
  (->* (human? symbol? rss-dialect?) (#:elem-prefix (or/c symbol? #f)) txexpr?)
  (match-define (list name-tag email-tag uri-tag)
    (cond [prefix (map (λ (s) (string->symbol (format "~a~a" prefix s))) '(name email uri))]
          [else '(name email uri)]))
  (case dialect
    [(atom)
     (define uri (if (human-uri p) `((,uri-tag ,(human-uri p))) `()))
     (txexpr entity '() `((,name-tag ,(human-name p))
                          (,email-tag ,(human-email p))
                          ,@uri))]
    [(rss)
     (txexpr entity '() (list (format "~a (~a)" (human-email p) (human-name p))))]))
  
(module+ test
  (define joel (person "Joel" "joel@msn.com"))
  (check-true (human? joel))
  (check-equal? (person->xexpr joel 'author 'rss) '(author "joel@msn.com (Joel)"))
  (check-equal? (person->xexpr joel 'author 'atom) '(author (name "Joel") (email "joel@msn.com")))

  ;; Prefixing child elements
  (check-equal? (person->xexpr joel 'owner 'rss #:elem-prefix 'itunes:) '(owner "joel@msn.com (Joel)"))
  (check-equal? (person->xexpr joel 'itunes:owner 'atom #:elem-prefix 'itunes:)
                '(itunes:owner (itunes:name "Joel") (itunes:email "joel@msn.com"))))

;; ~~ MIME types ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; This code is adapted from koyo/mime by Bogdan Popa.
;; (Simply to avoid having a large dependency just for MIME typing)
;; https://github.com/Bogdanp/koyo/blob/a1ce05497e72357c03b868d3aa3b9b40f462227c/koyo-lib/koyo/mime.rkt
;; Licensed under the 3-clause BSD License.

(define-runtime-path mime.types-path
  (build-path "private/mime.types"))

(define path->mime-type
  (make-path->mime-type mime.types-path))

;; Return a MIME type for a given file extension.
;; MIME types are not required; if unknown, the type should not be specified.
(define (ext->mime-type ext)
  (match (path->mime-type (string->path ext))
    [(? bytes? b) (bytes->string/utf-8 b)]
    [_ #f]))

(module+ test
  ;; Check some common types
  (check-equal? (ext->mime-type ".mp3") "audio/mpeg")
  (check-equal? (ext->mime-type ".m4a") "audio/x-m4a")
  (check-equal? (ext->mime-type ".mpg") "video/mpeg")
  (check-equal? (ext->mime-type ".mp4") "video/mp4")

  ;; Empty list returned for unknown extensions
  (check-equal? (ext->mime-type ".asdahsf") #f))

;; ~~ Enclosures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(struct enclosure (url type size)
  #:guard (struct-guard/c valid-url-string? (or/c non-empty-string? #f) exact-nonnegative-integer?)
  #:methods gen:feed-data
  [(define/contract (express-xml e dialect _url #:as [r 'xexpr])
     (->* (any/c rss-dialect? any/c) (#:as symbol?) xexpr?)
     (match-define (enclosure url type size) e)
     (case dialect
       [(atom)
        `(link [[rel "enclosure"]
                ,@(if type `((type ,type)) '())
                [length ,(number->string size)]
                [href ,url]])]
       [(rss)
        `(enclosure [[url ,url]
                     [length ,(number->string size)]
                     ,@(if type `((type ,type)) '())])]))])

(module+ test
  (require racket/file)
  (check-exn exn:fail:contract? (λ () (enclosure "invalid-url" "audio/x-m4a" 1234)))
  (check-exn exn:fail:contract? (λ () (enclosure "http://d.com/f.e" -900 1234)))
  (check-exn exn:fail:contract? (λ () (enclosure "http://d.com/f.e" "audio/x-m4a" -1)))

  (define test-enc
    (enclosure "gopher://umn.edu/greeting.m4a" "audio/x-m4a" 1234))

  (check-txexprs-equal?
   (express-xml test-enc 'atom #f)
   '(link [[rel "enclosure"]
           [href "gopher://umn.edu/greeting.m4a"]
           [length "1234"]
           [type "audio/x-m4a"]]))
  
  (check-txexprs-equal?
   (express-xml test-enc 'rss #f)
   '(enclosure [[url "gopher://umn.edu/greeting.m4a"]
                [length "1234"]
                [type "audio/x-m4a"]]))

  ;; Enclosure with unknown type
  (define test-enc2
    (enclosure "gopher://umn.edu/greeting.m4a" #f 1234))
  
  (check-txexprs-equal?
   (express-xml test-enc2 'atom #f)
   '(link [[rel "enclosure"]
           [href "gopher://umn.edu/greeting.m4a"]
           [length "1234"]]))
  
  (check-txexprs-equal?
   (express-xml test-enc2 'rss #f)
   '(enclosure [[url "gopher://umn.edu/greeting.m4a"]
                [length "1234"]])))

;; Convenient way to make an enclosure if you have an existing file
(define/contract (file->enclosure file-path base-url)
  (-> path-string? valid-url-string? enclosure?)
  (unless (eq? 'file (file-or-directory-type file-path))
    (raise-argument-error 'file->enclosure "path to an existing file" file-path))
  (define filename (car (reverse (explode-path file-path))))
  (enclosure (path->string (build-path (string->path base-url) filename))
             (ext->mime-type (path->string filename))
             (file-size file-path)))