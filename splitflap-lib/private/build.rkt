#lang racket/base

(require json
         net/url
         racket/port
         racket/runtime-path
         racket/string)

;; Utility file, not loaded by the rest of the library, used only for infrequent
;; manual updates of entities.rktd

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

(define (test-read-rktd)
  (with-input-from-file entities.rktd
    (lambda () (read))))

(module+ test)
