#lang racket/base

;;
;; Predicates and constructors for Dates, Persons, Tag URIs, URLs and Email addresses

(require (for-syntax racket/base)
         gregor
         racket/contract
         racket/generic
         racket/match
         racket/string
         txexpr
         net/url-string)

;; Interfaces

(provide dns-domain?
         valid-url-string?
         tag-entity-date?
         tag-specific-string?
         valid-email-address-string?
         
         mint-tag-uri
         tag-uri?
         tag-uri->string
         
         gen:validated-xexpr
         atom:element?
         ->xexpr
         ->xhtml
         
         Person
         atom:person?

         Date         
         atom:date?
         atom-tz-offset-hours
         chronological
         reverse-chronological)

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

; RFC 1035
;  <domain> ::= <subdomain> | " "
;  <subdomain> ::= <label> | <subdomain> "." <label>
;  <label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
;  <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
;  <let-dig-hyp> ::= <let-dig> | "-"
;  <let-dig> ::= <letter> | <digit>
;  <letter> ::= any one of the 52 alphabetic characters A through Z in
;  upper case and a through z in lower case
;  <digit> ::= any one of the ten digits 0 through 9
;
; The labels must follow the rules for ARPANET host names. They must
; start with a letter, end with a letter or digit, and have as interior
; characters only letters, digits, and hyphen.
;
; Per RFC 1035, each labels must be 63 characters or less including 1
; byte for a length header in front of each label; and the total name length
; must be 255 bytes or less including those header bytes.

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
  (check-false (dns-domain? (string-append longest-valid-domain "a")))
)

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
  (check-false (valid-url-string? "telnet://192.0.2.16:80/"))
)

; RFC 4151
; The general syntax of a tag URI, in ABNF [2], is:
;
;      tagURI = "tag:" taggingEntity ":" specific [ "#" fragment ]
;   Where:
;
;      taggingEntity = authorityName "," date
;      authorityName = DNSname / emailAddress
;      date = year ["-" month ["-" day]]
;      year = 4DIGIT
;      month = 2DIGIT
;      day = 2DIGIT
;      DNSname = DNScomp *( "."  DNScomp ) ; see RFC 1035 [3]
;      DNScomp = alphaNum [*(alphaNum /"-") alphaNum]
;      emailAddress = 1*(alphaNum /"-"/"."/"_") "@" DNSname
;      alphaNum = DIGIT / ALPHA
;      specific = *( pchar / "/" / "?" ) ; pchar from RFC 3986 [1]
;      fragment = *( pchar / "/" / "?" ) ; same as RFC 3986 [1]
;
; RFC 3986
;  pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
;  unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
;  sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
;                   / "*" / "+" / "," / ";" / "="
;
; RFC 2234
;  ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z
;  DIGIT          =  %x30-39             ; 0-9

; Validates the portion labeled “specific” in the RFC 4151 grammar
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
     (if mode (write-string "<tag-uri " port) (write-string "(tag-uri " port))
     ((case mode
        [(#t) write]
        [(#f) display]
        [else (lambda (p port) (print p port mode))])
      (tag-uri->string t) port)
     (if mode (write-string ">" port) (write-string ")" port)))])

(define/contract (mint-tag-uri authority date specific)
  (-> dns-domain? tag-entity-date? tag-specific-string? tag-uri?)
  (tag-uri authority date specific))

(define/contract (tag-uri->string t #:specific [specific (tag-uri-specific t)])
  (->* (tag-uri?) (#:specific tag-specific-string?) string?)
  (format "tag:~a,~a:~a" (tag-uri-authority t) (tag-uri-date t) specific))

;;
;; Email

(define-explained-contract (valid-email-address-string? str)
  "a valid RFC 5322 email address"
  (and (string? str)
       (regexp-match? #px"^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\\[\\]-\x7f]|\\\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\\])$" str)))

(define-generics validated-xexpr
  (->xexpr validated-xexpr)
  (->xhtml validated-xexpr)
  #:defaults ([xexpr?
               (define (->xexpr x) x)
               (define (->xhtml x) (xexpr->html x))]))

(struct atom:element (name attributes elements)
  #:methods gen:validated-xexpr
  [(define/generic super-construct ->xexpr)
   (define (->xexpr t)
     (txexpr (atom:element-name t) (atom:element-attributes t) (map super-construct (atom:element-elements t))))
   (define (->xhtml t)
     (xexpr->html (->xexpr t)))]
  #:methods gen:custom-write
  [(define (write-proc t port mode)
     (if mode (write-string "<atom:element " port) (write-string "(atom:element " port))
     ((case mode
        [(#t) write]
        [(#f) display]
        [else (lambda (p port) (print p port mode))])
      (atom:element-name t) port)
     (if mode (write-string ">" port) (write-string ")" port)))]
  #:guard (λ (name attrs elems instance)
            (unless (symbol? name)
              (raise-argument-error instance "symbol?" 0 name attrs elems))
            (unless (txexpr-attrs? attrs)
              (raise-argument-error instance "txexpr-attrs?" 1 name attrs elems))
            (unless (andmap validated-xexpr? elems)
              (raise-argument-error instance "(listof xexpr-struct?)" 2 name attrs elems))
            (values name attrs elems)))

;;
;; Dates

(define atom-tz-offset-hours (make-parameter 0))

(struct atom:date (val)
  #:methods gen:validated-xexpr
  [(define (->xexpr d) (~t (atom:date-val d) "yyyy-MM-dd'T'HH:mm:ssXXX"))
   (define (->xhtml d) (->xexpr d))]
  #:methods gen:custom-write
  [(define (write-proc d port mode)
     (when mode (write-string "<atom:date " port))
     ((case mode
        [(#t) write]
        [(#f) display]
        [else (lambda (p port) (print p port mode))])
      (string->symbol (moment->iso8601 (atom:date-val d))) port)
     (when mode (write-string ">" port)))])
    
(define (n: v) (cond [(not v) 0] [(string? v) (string->number v)] [else v]))

(define/contract (Date str)
  (-> string? atom:date?)
  (define date/time-regex
    #px"^([0-9]+)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])(?:\\s+([01]?[0-9]|2[0-3]):([0-5][0-9])(?::([0-5][0-9]|60))?)?")
  (match str
    [(pregexp date/time-regex (list _ y m d hr min sec))
     (atom:date (moment (n: y) (n: m) (n: d) (n: hr) (n: min) (n: sec) 0 #:tz (* 3600 (atom-tz-offset-hours))))]
    [_ (raise-argument-error 'Date "string in the format ‘YYYY-MM-DD [hh:mm[:ss]]’" str)]))

(define (date-compare op a b)
  (op (atom:date-val a) (atom:date-val b)))

(define/contract (chronological a b)
  (-> atom:date? atom:date? boolean?)
  (date-compare moment<? a b))

(define/contract (reverse-chronological a b)
  (-> atom:date? atom:date? boolean?)
  (date-compare moment>? a b))

;;
;; Persons
(struct atom:person atom:element ())

(define/contract (Person name [email #f] [uri #f])
  (->* (string?) ((or/c valid-email-address-string? #f) (or/c valid-url-string? #f)) atom:person?)
  (define uri-enc (and uri (url->string (string->url uri))))
  (atom:person 'person '() (filter txexpr-element? `((name ,name)
                                                     (email ,email)
                                                     (uri ,uri-enc)))))
