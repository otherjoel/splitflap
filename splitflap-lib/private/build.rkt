#lang racket/base

(require json
         net/url
         racket/file
         racket/list
         racket/match
         racket/port
         racket/runtime-path
         racket/string)

;; Utility file, not loaded by the rest of the library, used only for infrequent
;; manual updates of entities.rktd and mime-types.rktd

(define-runtime-path entities.rktd "./entities.rktd")

;; Download the W3C list of HTML entities and serialize as a hash table.
;; Converts:      "&Aacute": { "codepoints": [193], "characters": "\u00C1" }, ...
;; To:            #hash(("aacute" . (193)) ... )

;; Commented out to emphasize that this should only be run manually.
;; This function is never “armed” in the public package!
#;(define (download-entities)
  (define json-url (string->url "https://html.spec.whatwg.org/entities.json"))
  (define entities (string->jsexpr (port->string (get-pure-port json-url))))
  (with-output-to-file entities.rktd #:exists 'replace
    (lambda ()
      (write
       (for/hash ([(raw-entity v) (in-hash entities)])
         (define entity (string-trim (string-downcase (symbol->string raw-entity)) #rx"&|;"))
         (cond
           [(member entity '("amp" "lt" "gt" "quot" "apos"))
            ;; These five are valid in XML, so use symbols for their codepoint values
            (values entity (list (string->symbol entity)))] 
           [else (values entity (hash-ref v 'codepoints))]))))))

(define-runtime-path mime-types.rktd "./mime-types.rktd")

;; Download the Apache’s public domain list of MIME types and serialize as a hash table.
;; Converts:      application/epub+zip				epub
;; To:            #hasheq(("epub" . "application/epub+zip") ... )

;; Commented out to emphasize that this should only be run manually.
;; This function is never “armed” in the public package!
;; Check for recent updates: https://github.com/apache/httpd/commits/trunk/docs/conf/mime.types
#;(define (download-mime-types)
  (define mime-types-url (string->url "https://raw.githubusercontent.com/apache/httpd/refs/heads/trunk/docs/conf/mime.types"))
  (define mimetypes-lines (port->lines (get-pure-port mime-types-url)))
  (define extensions-table
    (let loop ([lines mimetypes-lines]
               [extensions (hasheq)])
      (cond
        [(null? lines) extensions]
        [(string-prefix? (car lines) "#") (loop (cdr lines) extensions)]
        [else
         (match-let* ([(cons line remaining) lines]
                      [(list mime-type exts ...) (regexp-match* #px"(\\S+)" line)])
           (define new-extensions (append-map (lambda (ext) (list (string->symbol ext) mime-type)) exts))
           (loop remaining (apply hash-set* extensions new-extensions)))])))
  
  (with-output-to-file mime-types.rktd #:exists 'replace
    (lambda () (write extensions-table))))

(define-runtime-path idna-tables.rktd "./idna-tables.rktd")

;; Download IANA’s IDNA2008 derived property table, plus the Unicode data needed by the
;; contextual rules (RFC 5892) and the Bidi rule (RFC 5893), all for one Unicode version.
;; Commented out to emphasize that this should only be run manually.
#;(define (download-idna-tables [unicode-version "12.0.0"])
  (define (fetch url)
    (port->lines (get-pure-port (string->url url))))
  (define (ucd-file name)
    (fetch (format "https://www.unicode.org/Public/~a/ucd/~a" unicode-version name)))
  (define (parse-ranges lines rx keep)
    (sort (filter-map (λ (line)
                        (match (regexp-match rx line)
                          [(list _ start end value)
                           (define v (keep value))
                           (define s (string->number start 16))
                           (and v (vector s (if end (string->number end 16) s) v))]
                          [#f #f]))
                      lines)
          < #:key (λ (r) (vector-ref r 0))))
  (define (merge ranges)
    (reverse
     (for/fold ([acc '()]) ([r (in-list ranges)])
       (match* (acc r)
         [((cons (vector s e v) more) (vector s2 e2 v2))
          #:when (and (equal? v v2) (= s2 (add1 e)))
          (cons (vector s e2 v) more)]
         [(_ _) (cons r acc)]))))
  (define (table lines rx keep)
    (list->vector (merge (parse-ranges lines rx keep))))
  (define ucd-rx #px"^([0-9A-F]+)(?:\\.\\.([0-9A-F]+))?\\s*;\\s*([^\\s#]+)")
  (define iana-rx #px"^([0-9A-F]+)(?:-([0-9A-F]+))?,([A-Z]+),")
  (define ((one-of . names) v) (and (member v names) (string->symbol v)))
  (define tables
    (hasheq 'unicode-version unicode-version
            'properties (table (fetch (format "https://www.iana.org/assignments/idna-tables-~a/idna-tables-properties.csv"
                                              unicode-version))
                               iana-rx
                               (one-of "PVALID" "CONTEXTJ" "CONTEXTO" "UNASSIGNED"))
            'bidi (table (ucd-file "extracted/DerivedBidiClass.txt") ucd-rx
                         (λ (v) (and (not (equal? v "L")) (string->symbol v))))
            'joining (table (ucd-file "extracted/DerivedJoiningType.txt") ucd-rx (one-of "D" "L" "R" "T"))
            'virama (table (ucd-file "extracted/DerivedCombiningClass.txt") ucd-rx
                           (λ (v) (and (equal? v "9") 'virama)))
            'scripts (table (ucd-file "Scripts.txt") ucd-rx
                            (one-of "Greek" "Hebrew" "Hiragana" "Katakana" "Han"))))
  (with-output-to-file idna-tables.rktd #:exists 'replace
    (lambda () (write tables))))

(module+ test)
