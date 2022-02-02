#lang racket/base

(require racket/runtime-path
         setup/getinfo)

(provide splitflap-version)

(define-runtime-path lib-folder "..")

(define (splitflap-version)
  ((get-info/full lib-folder) 'version))
