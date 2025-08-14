#lang at-exp racket/base

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
         (if (null? x) '("Apple’s Podcast feed requirements") x)))

(define (W3CFeedValidator . x)
  (apply hyperlink "https://validator.w3.org/feed/" (if (null? x) '("W3C Feed Validator") x)))

(define-runtime-path aux-css "styles/my.css")
(define-runtime-path aux-tex "styles/my.tex")
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

(define (callout . args)
  (paragraph (style "callout" (list (color-property (list #x01 #x46 #x6c))
                                    (css-style-addition aux-css)
                                    (alt-tag "div")
                                    (tex-addition aux-tex)))
             args))

(define-runtime-path flipboard-js "js/ticker-board.min.js")

(define flipboard-div
  (paragraph
   (style "flipboard"
          (list 'div
                (css-style-addition flappy-css)
                (js-addition flipboard-js)
                (tex-addition flappy-tex)
                (attributes '((id . "flappy")))))
   (list (elem ""))))

(define flipboard-script
  @(paragraph
    (style "flip-js" (list (alt-tag "script")))
    (list @literal|{
          new RotationBoard(document.getElementById('flappy'), {
          messages: ['splitflap', 'XML Feeds'],
          count: 1,
          size: 9,
          delay: 6000,
          })
          }|)))
