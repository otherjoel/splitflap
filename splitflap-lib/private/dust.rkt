#lang racket/base

(require (for-syntax racket/base)
         racket/port
         racket/string
         racket/match
         racket/list
         txexpr
         xml)

(provide (all-defined-out))

(module+ test
  (require rackunit))

;; ~~ XML Utility functions ~~~~~~~~~~~~~~~~~~~~~~

;; Create CDATA from a string or txexpr and undo any entity escaping in the result.
(define (as-cdata v)
  (define entities (hash "&amp;" "&" "&lt;" "<" "&gt;" ">"))
  (define cdata-str
    (cond [(txexpr? v)
           (regexp-replace* #rx"&amp;|&lt;|&gt;" (xexpr->string v) (lambda (e) (hash-ref entities e)))]
          [else v]))
  (cdata 'racket 'racket (format "<![CDATA[~a]]>" cdata-str)))

(module+ test
  ;; String input: no escaping in result
  (check-equal? (as-cdata "Hi & < >")
                (cdata 'racket 'racket "<![CDATA[Hi & < >]]>"))
  ;; Txexpr input: no escaping in result
  (check-equal? (as-cdata '(div (p "Hi & < >")))
                (cdata 'racket 'racket "<![CDATA[<div><p>Hi & < ></p></div>]]>")))

;; Convert an x-expression to a complete XML document
(define (xml-document xpr)
  (document
   (prolog (list (p-i 'racket 'racket 'xml "version=\"1.0\" encoding=\"UTF-8\"")) #f '())
   (xexpr->xml xpr)
   '()))

;; ~~ HTML entity escaping ~~~~~~~~~~~~~~~~~~~~~~~

;; Apple requires that the following characters not be used in PCDATA, even though they
;; are permitted by the XML spec:
;;
;;   #\' #\\ #\" #\© #\℗ #\™
;;
;; Instead, the must be referred to via predefined entity or character references.
;; See: https://podcasters.apple.com/support/823-podcast-requirements
;; (Apple does not give a reason for this requirement, and it groups them together
;; with #\&, #\<, and #\>, which are disallowed by the standard and thus replaced
;; automatically by the XML library.)
;;
;; So, we must find those symbols in text strings and extract the appropriate references.

;; escape-for-apple: (-> string? (listof (or/c string? symbol? valid-char?)))
(define (escape-for-apple str)
  (match (regexp-match-positions* #rx"['\"©℗™]" str)
    ;; Don't copy the string unless necessary:
    ['()
     (list str)]
    [to-escape
     (let loop ([gap-end (string-length str)]
                [to-escape (reverse to-escape)]
                [acc null])
       (match to-escape
         ['()
          (if (zero? gap-end)
              acc
              (cons (substring str 0 gap-end)
                    acc))]
         [(cons (cons pos gap-start) to-escape)
          (loop pos
                to-escape
                (cons (match (string-ref str pos)
                        [#\' 'apos]
                        [#\" 'quot]
                        [#\© #xA9]
                        [#\℗ #x2117]
                        [#\™ #x2122])
                      (if (= gap-start gap-end)
                          acc
                          (cons (substring str gap-start gap-end)
                                acc))))]))]))

(module+ test
  ;; Cases to cover:
  ;;   - first/last character needing/not nedding to be escaped
  ;;   - two adjacent characters needing to be escaped
  ;;   - avoid pointless copies
  (define test-str1 "Punch & Judy's friend \"George\"")
  (define test-str2 "™<h1>©℗ Product")
  (check-equal? (escape-for-apple test-str1)
                '("Punch & Judy" apos "s friend " quot "George" quot))
  (check-equal? (escape-for-apple test-str2)
                '(#x2122 "<h1>" #xA9 #x2117 " Product"))
  (let ([already-sanitary-string "abcdefg"])
    (check-eq? (car (escape-for-apple already-sanitary-string))
               already-sanitary-string))
  ;; Check that we've typed the non-ASCII characters properly:
  (for ([int (in-list '(#xA9 #x2117 #x2122))])
    (define char (integer->char int))
    (with-check-info (('valid-char? int)
                      ('char? char))
      (check-equal? (escape-for-apple (string char))
                    (list int)
                    "character references should round-trip")))
  )

;; Recursively pre-escape any string entities in an x-expression that
;; are direct children of an element whose ‘type’ attribute is “text”
(define (txexpr->apple-txexpr x)
  (cond 
    [(and (txexpr? x) (equal? "text" (attr-ref x 'type #f)))
     (txexpr
      (car x)
      (get-attrs x)
      (append-map (λ (c)
                    (if (string? c)
                        (escape-for-apple c)
                        (list (txexpr->apple-txexpr c))))
                  (get-elements x)))]
    [(txexpr? x) (txexpr (car x) (get-attrs x) (map txexpr->apple-txexpr (get-elements x)))]
    [else x]))

(module+ test
  (define judy-str "Judy's friend \"George\"")
  ;; no effect without '(type "text") in attrs
  (check-equal? (txexpr->apple-txexpr `(div (div [[id "t's"]] ,judy-str (div ,judy-str))))
                `(div (div [[id "t's"]] ,judy-str (div ,judy-str))))

  ;; escapes with '(type "text") in attrs, but only immediate child strings
  (check-equal? (txexpr->apple-txexpr `(div (div [[type "text"]] ,judy-str (div ,judy-str))))
                `(div (div [[type "text"]] "Judy" apos "s friend " quot "George" quot (div ,judy-str)))))

;; ~~ XML Display ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Convert an xexpr to a string with escaped and nicely-indented XML.
(define (indented-xml-string xpr #:document? [doc? #f])
  (define x
    (let ([pxpr (txexpr->apple-txexpr xpr)])
      (if doc? (xml-document pxpr) (xexpr->xml pxpr))))
  (define display-proc (if doc? display-xml display-xml/content))
  (define xml-str
    (with-output-to-string
      (λ ()
        (parameterize ([empty-tag-shorthand 'always])
          (display-proc x #:indentation 'peek)))))
  (string-trim xml-str #:right? #f))

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

;; Handy for splicing something or nothing into xexprs
(define-syntax (if/sp stx)
  (syntax-case stx ()
    [(_ check val) #'(if check (list val) '())]))