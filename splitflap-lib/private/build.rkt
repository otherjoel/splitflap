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
#;(define (download-mime-types)
  (define mime-types-url (string->url "https://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types"))
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

(module+ test)
