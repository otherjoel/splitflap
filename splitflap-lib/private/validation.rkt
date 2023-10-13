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

;; ASIDE: Here’s a nearly-complete Regex for RFC5322. Not using it because it allows
;; for things that many/most email services & clients don’t actually support.
;; #px"^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\\[\\]-\x7f]|\\\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\\])$" str)))

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
       (regexp-match? #rx"^[a-zA-Z0-9_.~,;=$&'@\\!\\(\\)\\*\\+\\:\\?\\/\\-]*$" val)))

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



;; ~~ Dates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (n: v) (cond [(not v) 0] [(string? v) (string->number v)] [else v]))

(define date/time-regex
    #px"^([0-9]+)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])(?:\\s+([01]?[0-9]|2[0-3]):([0-5][0-9])(?::([0-5][0-9]|60))?)?")

(define (infer-moment [str ""])
  (match str
    ["" (now/moment)]
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

;; Convenient way to make an enclosure if you have an existing file
(define/contract (file->enclosure file-path base-url)
  (-> path-string? valid-url-string? enclosure?)
  (unless (eq? 'file (file-or-directory-type file-path))
    (raise-argument-error 'file->enclosure "path to an existing file" file-path))
  (define filename (car (reverse (explode-path file-path))))
  (enclosure (path->string (build-path (string->path base-url) filename))
             (path/string->mime-type filename)
             (file-size file-path)))



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