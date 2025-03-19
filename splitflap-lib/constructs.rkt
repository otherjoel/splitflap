#lang racket/base

;;
;; Predicates and constructors for Dates, Persons, Tag URIs, URLs, email addresses and enclosures

(require "private/validation.rkt"
         (only-in "private/dust.rkt" txexpr?)
         gregor
         racket/contract)

(provide dns-domain?
         valid-url-string?
         email-address?
         validate-email-address

         (contract-out
          [url-domain (-> valid-url-string? dns-domain?)]
          [url-join (-> valid-url-string? relative-path? valid-url-string?)])
         
         ; Tag URIs:
         tag-entity-date?
         tag-uri?
         tag-specific-string?
         (contract-out
          [mint-tag-uri (-> (or/c dns-domain? email-address?) tag-entity-date? tag-specific-string? tag-uri?)]
          [append-specific (-> tag-uri? tag-specific-string? tag-uri?)]
          [tag-uri->string (->* (tag-uri?) (#:specific tag-specific-string?) string?)]
          [tag=? (-> tag-uri? tag-uri? boolean?)]
          [normalize-tag-specific (-> string? tag-specific-string?)])

         ; RSS Dialects
         rss-dialect?
         
         ; Persons
         person?
         (contract-out
          [person (->* (string? email-address?) ((or/c valid-url-string? #f)) person?)]
          [person->xexpr (-> person? symbol? (or/c rss-dialect? 'itunes) txexpr?)])
         
         ; Moments
         (contract-out
          [infer-moment (->* () (string?) moment?)]
          [moment->string (-> moment? (or/c 'rss 'atom) string?)])             
         
         ; Enclosures and MIME types
         (struct-out enclosure)
         file->enclosure
         (contract-out
          [mime-types-by-ext (promise/c (hash/c symbol? string? #:immutable #t))]
          [path/string->mime-type (-> path-string? (or/c string? #f))])
         
         ; Language codes
         language-codes
         iso-639-language-code?
         (contract-out
          [system-language (promise/c iso-639-language-code?)]))
