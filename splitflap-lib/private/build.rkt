#lang racket/base

(require json
         net/url
         racket/port
         racket/runtime-path
         racket/string)

;; Utility file: download he W3C list of HTML entities, convert the keys to

(define-runtime-path entities.rktd "./entities.rktd")

(define (download-entities)
  (define json-url (string->url "https://html.spec.whatwg.org/entities.json"))
  (define entities (string->jsexpr (port->string (get-pure-port json-url))))
  (with-output-to-file entities.rktd #:exists 'replace
    (lambda ()
      (write
       (for/hash ([(entity v) (in-hash entities)])
         (values (string-trim (string-downcase (symbol->string entity)) #rx"&|;") v))))))

(define (test-read-rktd)
  (with-input-from-file entities.rktd
    (lambda () (read))))

(module+ test)
