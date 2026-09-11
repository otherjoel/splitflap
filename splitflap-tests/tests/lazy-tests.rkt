#lang racket/base

(require rackunit)

(parameterize ([current-namespace (make-base-namespace)])
  (namespace-require 'splitflap)
  (eval '(parameterize ([feed-language 'en])
           (express-xml (feed (mint-tag-uri "example.com" "2024" "blog")
                              "https://example.com/"
                              "Blog"
                              (list (feed-item (mint-tag-uri "example.com" "2024" "blog.1")
                                               "https://example.com/1.html"
                                               "One"
                                               (person "Kate" "kate@example.com")
                                               (infer-moment "2024-01-01")
                                               (infer-moment "2024-01-01")
                                               '(p "Hi"))))
                        'atom
                        "https://example.com/feed.atom")))
  (check-false (module-declared? 'splitflap/private/idna #f))
  (check-false (module-declared? 'splitflap/private/mime-types #f))
  (eval '(domain->ascii "bücher.example"))
  (check-true (module-declared? 'splitflap/private/idna #f))
  (eval '(path/string->mime-type "episode.mp3"))
  (check-true (module-declared? 'splitflap/private/mime-types #f)))
