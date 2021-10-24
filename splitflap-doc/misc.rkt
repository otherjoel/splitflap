#lang racket/base

(require racket/runtime-path
         scribble/core
         scribble/html-properties
         scribble/latex-properties
         scribble/manual)

(provide (all-defined-out))

(define (Atom1.0 . x)
  (apply hyperlink "https://datatracker.ietf.org/doc/html/rfc4287" (if (null? x) '("Atom 1.0") x)))

(define (RSS2.0 . x)
  (apply hyperlink "https://www.rssboard.org/rss-specification" (if (null? x) '("RSS 2.0") x)))

(define (AppleRequirements . x)
  (apply hyperlink "https://podcasters.apple.com/support/823-podcast-requirements"
         (if (null? x) '("Apple Podcast feed requirements") x)))


(define-runtime-path aux-css "styles/terminal.css")
(define-runtime-path aux-tex "styles/terminal.tex")
(define-runtime-path flappy-css "styles/flappy.css")
(define-runtime-path flappy-tex "styles/flappy.tex")

(define (terminal . args)
  (compound-paragraph (style "terminal" (list (color-property (list #x66 #x33 #x99))
                                              (css-style-addition aux-css)
                                              (alt-tag "div")
                                              (tex-addition aux-tex)))
                      (list (apply verbatim args))))

;; Simulate a command-line prompt
(define (:> . args)
  (element (style "prompt" (list (color-property (list #x66 #x66 #x66))))
           (apply exec (cons "> " args))))

;; Simulate a bash-style comment
(define (rem . args)
  (apply racketcommentfont (cons "# " args)))

(define (spec . args)
  (element (style "special" (list (color-property (list #x66 #x33 #x99)))) args))

(define-runtime-path flipboard-js "js/ticker-board.min.js")
(define-runtime-path ticker-js "js/splitflap-ticker.js")

(define flipboard
  (paragraph
   (style "flipboard"
          (list 'div
                (css-style-addition flappy-css)
                (js-addition flipboard-js)
                (tex-addition flappy-tex)
                (attributes '((id . "flappy")))))
   (list (elem ""))))
