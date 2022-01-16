#lang racket/base

(require "dust.rkt"
         "xml-generic.rkt"
         gregor
         net/url-string
         racket/contract
         racket/match
         racket/promise
         racket/runtime-path
         racket/string
         xml)

(provide dns-domain?
         email-address?
         validate-email-address
         tag-entity-date?
         tag-specific-string?
         tag-uri?
         mint-tag-uri
         append-specific
         tag-uri->string
         tag=?
         infer-moment
         moment->string
         (rename-out [make-person person])
         person?
         person->xexpr
         rss-dialect?
         (struct-out enclosure)
         file->enclosure
         mime-types-by-ext
         path/string->mime-type
         valid-url-string?
         iso-639-language-code?
         language-codes
         system-language)

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
  (check-false (dns-domain? (string-append longest-valid-domain "a"))))

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
  "a string containing only a–z, A–Z, 0–9 or chars in the set -._~!$&'()*+,;=:@/?"
  (and (string? val)
       (regexp-match? #rx"^[a-zA-Z0-9_.~,;=$&'@\\!\\(\\)\\*\\+\\:\\?\\/\\-]*$" val)))

(module+ test
  (check-true (tag-specific-string? "abcdefghijklmnopqrstuvwxyz0123456789"))
  (check-true (tag-specific-string? "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (check-true (tag-specific-string? "_.~,;=$&'@"))
  (check-true (tag-specific-string? "!()*+:/-")))

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

(define (mint-tag-uri authority date specific)
  (tag-uri authority date specific))

(define (append-specific t suffix)
  (struct-copy tag-uri t [specific (string-append (tag-uri-specific t) "." suffix)]))

(define (tag-uri->string t #:specific [specific (tag-uri-specific t)])
  (string-append "tag:" (tag-uri-authority t) "," (tag-uri-date t) ":" specific))


;; RFC 4151 section 2.4 — Equality of tags:
;; “Tags are simply strings of characters and are considered equal if and
;;  only if they are completely indistinguishable in their machine
;;  representations when using the same character encoding.  That is, one
;;  can compare tags for equality by comparing the numeric codes of their
;;  characters, in sequence, for numeric equality.  This criterion for
;;  equality allows for simplification of tag-handling software, which
;;  does not have to transform tags in any way to compare them.”

(define (tag=? t1 t2)
  (apply equal? (map tag-uri->string (list t1 t2))))

(module+ test
  (check-true (tag=? (mint-tag-uri "example.com" "2005" "main")
                     (mint-tag-uri "example.com" "2005" "main")))
  ;; Comparison is case-sensitive?
  (check-false (tag=? (mint-tag-uri "Example.com" "2005" "main")
                      (mint-tag-uri "example.com" "2005" "main")))
  ;; Date equivalency doesn’t count
  (check-false (tag=? (mint-tag-uri "Example.com" "2005-01" "main")
                      (mint-tag-uri "Example.com" "2005-01-01" "main"))))

;; ~~ Dates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (n: v) (cond [(not v) 0] [(string? v) (string->number v)] [else v]))

(define (infer-moment str)
  (define date/time-regex
    #px"^([0-9]+)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])(?:\\s+([01]?[0-9]|2[0-3]):([0-5][0-9])(?::([0-5][0-9]|60))?)?")
  (match str
    [(pregexp date/time-regex (list _ y m d hr min sec))
     (moment (n: y) (n: m) (n: d) (n: hr) (n: min) (n: sec) 0)]
    [_ (raise-argument-error 'Date "string in the format ‘YYYY-MM-DD [hh:mm[:ss]]’" str)]))

(define (moment->string m type)
  (~t m (case type
          [(atom) "y-MM-dd'T'HH:mm:ssXXXXX"]
          [(rss) "E, d MMM y HH:mm:ss xx"])))

;; ~~ Flavors of RSS: 'rss or 'atom ~~~~~~~~~~~~~~
(define-explained-contract (rss-dialect? v)
  "A symbol representing a valid RSS dialect: 'rss or 'atom"
  (or (eq? v 'rss) (eq? v 'atom)))


;; ~~ Persons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(struct person (name email uri) #:constructor-name person_)

(define (make-person name email [uri #f])
  (person_ name email uri))
  
(define (person->xexpr p entity dialect)
  (match-define (list name-tag email-tag uri-tag)
    (cond [(eq? dialect 'itunes) '(itunes:name itunes:email itunes:uri)]
          [else '(name email uri)]))
  (match-define (person name email uri) p)
  (case dialect
    [(atom itunes)
     `(,entity (,name-tag ,name)
               (,email-tag ,email)
               ,@(if/sp uri `(,uri-tag ,uri)))]
    [(rss)
     `(,entity ,(format "~a (~a)" email name))]))
  
(module+ test
  (define joel (make-person "Joel" "joel@example.com"))
  (check-true (person? joel))
  (check-equal? (person->xexpr joel 'author 'rss) '(author "joel@example.com (Joel)"))
  (check-equal? (person->xexpr joel 'author 'atom) '(author (name "Joel") (email "joel@example.com")))

  ;; Prefixing child elements
  (check-equal? (person->xexpr joel 'itunes:owner 'itunes)
                '(itunes:owner (itunes:name "Joel") (itunes:email "joel@example.com"))))


;; ~~ MIME types ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-runtime-path mime-types.rktd "mime-types.rktd")

(define mime-types-by-ext
  (delay/sync (with-input-from-file mime-types.rktd (lambda () (read)))))

;; Return a MIME type for a given file extension.
;; MIME types are not required; if unknown, the type should not be specified.
(define (path/string->mime-type p)
  (match (if (path? p) (path->string p) p)
    [(regexp #rx".*\\.([^\\.]*$)" (list _ ext))
     (hash-ref (force mime-types-by-ext)
               (string->symbol (string-downcase ext))
               #f)]
    [_ #f]))

(module+ test
  ;; Check some common types
  (check-equal? (path/string->mime-type ".mp3") "audio/mpeg")
  (check-equal? (path/string->mime-type ".m4a") "audio/mp4")
  (check-equal? (path/string->mime-type ".mpg") "video/mpeg")
  (check-equal? (path/string->mime-type ".mp4") "video/mp4")

  ;; Can use paths
  (check-equal? (path/string->mime-type (build-path "." "file.epub")) "application/epub+zip")

  ;; Empty list returned for unknown extensions
  (check-equal? (path/string->mime-type ".asdahsf") #f))

;; ~~ Enclosures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(struct enclosure (url type size)
  #:guard (struct-guard/c valid-url-string? (or/c non-empty-string? #f) exact-nonnegative-integer?)
  #:methods gen:food
  [(define/contract (express-xml e dialect [_url #f] #:as [r 'xml-string])
     (->* (any/c rss-dialect?) (any/c #:as symbol?) xexpr?)
     (match-define (enclosure url type size) e)
     (define encl-xpr
       (case dialect
         [(atom)
          `(link [[rel "enclosure"]
                  [href ,url]
                  [length ,(number->string size)]
                  ,@(if type `((type ,type)) '())])]
         [(rss)
          `(enclosure [[url ,url]
                       [length ,(number->string size)]
                       ,@(if type `((type ,type)) '())])]))
     (case r
       [(xexpr xexpr-cdata) encl-xpr]
       [(xml) (xexpr->xml encl-xpr)]
       [(xml-string) (indented-xml-string encl-xpr)]))])

(module+ test
  (require racket/file)
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
                [length "1234"]])))

;; Convenient way to make an enclosure if you have an existing file
(define/contract (file->enclosure file-path base-url)
  (-> path-string? valid-url-string? enclosure?)
  (unless (eq? 'file (file-or-directory-type file-path))
    (raise-argument-error 'file->enclosure "path to an existing file" file-path))
  (define filename (car (reverse (explode-path file-path))))
  (enclosure (path->string (build-path (string->path base-url) filename))
             (path/string->mime-type filename)
             (file-size file-path)))

(module+ test
  (require racket/file racket/runtime-path)
  (define-runtime-path temp "temp.mp3")
  (display-to-file (make-bytes 100 65) temp #:exists 'truncate)
  (check-equal?
   (express-xml (file->enclosure temp "http://example.com") 'atom #:as 'xexpr)
   '(link [[rel "enclosure"]
           [href "http://example.com/temp.mp3"]
           [length "100"]
           [type "audio/mpeg"]]))
  (delete-file temp))


;; ~~ ISO 639 Language codes ~~~~~~~~~~~~~~~~~~~~~

(define language-codes
  '(ab aa af ak sq am ar an hy as av ae ay az bm ba eu be bn bh bi bs br bg my ca km ch ce ny zh cu cv kw co cr hr cs da dv nl dz en eo et ee fo fj fi fr ff gd gl lg ka de ki el kl gn gu ht ha he hz hi ho hu is io ig id ia ie iu ik ga it ja jv kn kr ks kk rw kv kg ko kj ku ky lo la lv lb li ln lt lu mk mg ms ml mt gv mi mr mh ro mn na nv nd ng ne se no nb nn ii oc oj or om os pi pa ps fa pl pt qu rm rn ru sm sg sa sc sr sn sd si sk sl so st nr es su sw ss sv tl ty tg ta tt te th bo ti to ts tn tr tk tw ug uk ur uz ve vi vo wa cy fy wo xh yi yo za zu))

(define-explained-contract (iso-639-language-code? v)
  "ISO 639 language code symbol"
  (and (symbol? v) (memq v language-codes) #t))

(define system-language 
  (delay/sync
   (match (case (system-type 'os)
            [(unix macosx)
             (match (system-language+country)
               [(? bytes? b) (bytes->string/utf-8 b)] ; Bug in pre-8.4 CS returns bytes
               [(var v) v])]
            [(windows)
             ((dynamic-require 'file/resource 'get-resource)
              "HKEY_CURRENT_USER" "Control Panel\\International\\LocaleName")])
     [(? string? loc)
      (string->symbol (string-downcase (substring loc 0 2)))]
     [(var v)
      (raise
       (exn:fail:unsupported
        (format "attempt to determine system language resulted in: ~a" v)
        (current-continuation-marks)))])))