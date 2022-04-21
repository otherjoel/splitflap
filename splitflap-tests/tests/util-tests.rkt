#lang racket/base

(require rackunit
         splitflap/private/dust
         xml)

;; Unit tests for internal splitflap functions

;; ~~ Tagged X-expressions ~~~~~~~~~~~~~~~~~~~~~~~

(check-equal? (get-attrs '(br)) '())
(check-equal? (get-attrs '(div "hello")) '())
(check-equal? (get-attrs '(div amp "hello")) '())
(check-equal? (get-attrs '(div [[id "main"]] amp "hello")) '((id "main")))

(check-equal? (get-elements '(br)) '())
(check-equal? (get-elements '(div amp "hello")) '(amp "hello"))
(check-equal? (get-elements '(div [[id "main"]])) '())
(check-equal? (get-elements '(div [[id "main"]] amp "hello")) '(amp "hello"))



;; ~~ XML Utility functions ~~~~~~~~~~~~~~~~~~~~~~

;; String input: no escaping in result
(check-equal? (as-cdata "Hi & < >")
              (cdata 'racket 'racket "<![CDATA[Hi & < >]]>"))
;; Txexpr input: no escaping in result
(check-equal? (as-cdata '(div (p "Hi & < >")))
              (cdata 'racket 'racket "<![CDATA[<div><p>Hi & < ></p></div>]]>"))
;; …but DO escape the forbidden string "]]>" which would prematurely end the block
(check-equal? (as-cdata '(div (p "Hi ]]> whoops how did that get there")))
              (cdata 'racket 'racket "<![CDATA[<div><p>Hi ]]&gt; whoops how did that get there</p></div>]]>"))



;; ~~ HTML entity escaping ~~~~~~~~~~~~~~~~~~~~~~~

;; The XML 5 get replaced with symbolic equivalents
;; First/last entities and adjacent entities get replaced properly
(check-equal? (numberize-named-entities "&amp;&quot;&apos;&lt;&gt;")
              '(amp quot apos lt gt))

;; Others get replaced with numeric entities
(check-equal? (numberize-named-entities "Copyright&copy; Trademark&trade; Recording copyright&copysr;")
              '("Copyright" 169 " Trademark" #x2122 " Recording copyright" #x2117))
  
;; Entities with multiple codepoints get both inserted
(check-equal? (numberize-named-entities "This is one with multiple codepoints: &sqcaps;")
              '("This is one with multiple codepoints: " 8851 65024))

;; Non-existent entities get left in
(check-equal? (numberize-named-entities "Total &madeup; mistake")
              '("Total " "&madeup;" " mistake"))

;; Avoid pointless copies
(let ([already-sanitary-string "abcdefg"])
  (check-eq? (car (numberize-named-entities already-sanitary-string))
             already-sanitary-string))

;; Conversion to string
(define judy-str "Punch &amp; Judy&apos;s friend George&trade;")
(check-equal? (txexpr->safe-content-str `(div (p ,judy-str)))
              "<div><p>Punch &amp; Judy&apos;s friend George&#8482;</p></div>")

(define content2 "<div>Hi ]]> whoops how did that get there</div>")

;; Includes type=text attribute for Atom
(check-equal? (content->safe-element `(div (p "My trademark&trade;")) 'content 'atom #f)
              '(content ((type "html")) "<div><p>My trademark&#8482;</p></div>"))

;; Omit type attribute for RSS
(check-equal? (content->safe-element `(div (p "My trademark&trade;")) 'description 'rss #f)
              '(description "<div><p>My trademark&#8482;</p></div>"))

;; String content encoded as CDATA
(check-equal? (content->safe-element "<div>mischief managed: ]]></div>" 'content 'atom #f)
              '(content "<![CDATA[<div>mischief managed: ]]&gt;</div>]]>"))

;; preserve-cdata-struct? #t
(check-true (cdata? (cadr (content->safe-element "x" 'content 'atom #t))))



;; ~~ XML Display ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define test-xpr
  `(feed (author (name "Punch & Judy"))
         (title [[type "text"]] "Punch & Judy's <friend> \"George\" Escapes!&trade;")
         (subtitle "Here & < > are escaped, but ' \" © ℗ ™ are not")
         (content [[type "html"]] ,(txexpr->safe-content-str `(div ,judy-str)))))
(define expect-str #<<XML
<feed>
  <author>
    <name>Punch &amp; Judy</name>
  </author>
  <title type="text">Punch &amp; Judy's &lt;friend&gt; "George" Escapes!&amp;trade;</title>
  <subtitle>Here &amp; &lt; &gt; are escaped, but ' " © ℗ ™ are not</subtitle>
  <content type="html">&lt;div&gt;Punch &amp;amp; Judy&amp;apos;s friend George&amp;#8482;&lt;/div&gt;</content>
</feed>
XML
  )
(check-equal? (indented-xml-string test-xpr) expect-str)