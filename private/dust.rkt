#lang racket/base

(require racket/port
         racket/string
         txexpr
         xml)

(provide (all-defined-out))

(module+ test
  (require rackunit))

;; ~~ XML Utility functions ~~~~~~~~~~~~~~~~~~~~~~

;; Quick CDATA creation
(define (as-cdata v)
  (cdata #f #f (format "<![CDATA[~a]]>"
                       (cond [(txexpr? v) (xexpr->string v)]
                             [else v]))))

;; Convert an x-expression to a complete XML document
(define (xml-document xpr)
  (document
   (prolog (list (p-i 'racket 'racket 'xml "version=\"1.0\" encoding=\"UTF-8\"")) #f '())
   (xexpr->xml xpr)
   '()))

;; ~~ HTML entity escaping ~~~~~~~~~~~~~~~~~~~~~~~

;; The xml display functions take care of escaping #\& #\< and #\> inside content strings.
;; However we need to be able to escape other characters in some circumstances (for podcast
;; feeds in particular) and those escapes themselves include the ampersand character.
;; The solution is to pre-escape the extra characters immediately before converting to xml,
;; and then unescape the ampersands after converting the xml to a string.

;; This is the set of characters (besides #\& #\< and #\>) that Apple gets snippy about.
;; See https://podcasters.apple.com/support/823-podcast-requirements
(define string->entity
  (hash 
   "'" "%amp%apos;"
   "\"" "%amp%quot;"
   "©" "%amp%#xA9;"
   "℗" "%amp%#x2117;"
   "™" "%amp%#x2112;"))

(define (pre-escape-entities str)
  (regexp-replace*
   #rx"[&<>'\"©℗™]"
   str
   (lambda (match-str)
     (hash-ref string->entity match-str match-str))))

(define (post-escape-entities str)
  (string-replace str "%amp%" "&"))

(module+ test
  (define test-str1 "Punch & Judy's friend \"George\"")
  (define test-str2 "<h1>© ℗ Product™")
  (check-equal? (pre-escape-entities test-str1)
                "Punch & Judy%amp%apos;s friend %amp%quot;George%amp%quot;")
  (check-equal? (pre-escape-entities test-str2)
                "<h1>%amp%#xA9; %amp%#x2117; Product%amp%#x2112;")

  (check-equal? (post-escape-entities (pre-escape-entities test-str1))
                "Punch & Judy&apos;s friend &quot;George&quot;")
  (check-equal? (post-escape-entities (pre-escape-entities test-str2))
                "<h1>&#xA9; &#x2117; Product&#x2112;")
  )

;; Recursively pre-escape any string entities in an x-expression that
;; are direct children of an element whose ‘type’ attribute is “text”
(define (pre-escape x)
  (cond 
    [(and (txexpr? x) (eq? "text" (attr-ref x 'type #f)))
     (txexpr
      (car x)
      (get-attrs x)
      (map (λ (c) (if (string? c) (pre-escape-entities c) (pre-escape c))) (get-elements x)))]
    [(txexpr? x) (txexpr (car x) (get-attrs x) (map pre-escape (get-elements x)))]
    [else x]))

(module+ test
  (define judy-str "Judy's friend \"George\"")
  ;; no effect without '(type "text") in attrs
  (check-equal? (pre-escape `(div [[id "t's"]] ,judy-str (div ,judy-str)))
                `(div [[id "t's"]] ,judy-str (div ,judy-str)))

  ;; escapes with '(type "text") in attrs, but only immediate child strings
  (check-equal? (pre-escape `(div [[type "text"]] ,judy-str (div ,judy-str)))
                `(div [[type "text"]] "Judy%amp%apos;s friend %amp%quot;George%amp%quot;" (div ,judy-str))))

;; ~~ XML Display ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Convert an xexpr to a string with escaped and nicely-indented XML.
(define (indented-xml-string xpr #:document? [doc? #f])
  (define x
    (let ([pxpr (pre-escape xpr)])
      (if doc? (xml-document pxpr) (xexpr->xml pxpr))))
  (define display-proc (if doc? display-xml display-xml/content))
  (define xml-str
    (with-output-to-string
      (λ ()
        (parameterize ([empty-tag-shorthand (cons 'atom:link html-empty-tags)])
          (display-proc x #:indentation 'peek)))))
  (string-trim (post-escape-entities xml-str) #:right? #f))

(module+ test
  (define test-xpr
    `(feed (author (name "Punch & Judy"))
           (title [[type "text"]] "Punch & Judy's <friend> \"George\" Escapes!")
           (notes (div "Here & < > are escaped, but ' \" © ℗ ™ are not"))))
  (define expect-str #<<XML
<feed>
  <author>
    <name>Punch &amp; Judy</name>
  </author>
  <title type="text">Punch &amp; Judy&apos;s &lt;friend&gt; &quot;George&quot; Escapes!</title>
  <notes>
    <div>Here &amp; &lt; &gt; are escaped, but ' " © ℗ ™ are not</div>
  </notes>
</feed>
XML
    )
  (check-equal? (indented-xml-string test-xpr) expect-str))

;; ~~ Other stuff ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Build the <generator> tag
(define (generator dialect)
  (define gen-str (format "Racket v~a [~a]" (version) (system-type 'gc)))
  (case dialect
    [(rss) `(generator ,(string-append gen-str " (https://racket-lang.org)"))]
    [(atom) `(generator [[uri "https://racket-lang.org"] [version ,(version)]] ,gen-str)]))

;; Parameter determines whether feed output will include a <generator> tag
;; Useful for keeping unit tests from breaking on different versions
(define include-generator? (make-parameter #t))