#lang racket/base

(require racket/format
         racket/include
         racket/list
         racket/match
         racket/string
         (only-in splitflap/private/dust
                  ascii-string?
                  domain-problem
                  domain-problem-message
                  domain-problem-details))

(provide punycode-encode
         punycode-decode
         u-label->a-label
         a-label->u-label
         bidi-rule-violation)

;; ~~ Punycode (RFC 3492) ~~~~~~~~~~~~~~~~~~~~~~~~

(define base 36)
(define tmin 1)
(define tmax 26)
(define skew 38)
(define damp 700)
(define initial-bias 72)
(define initial-n 128)

(define (adapt delta numpoints first?)
  (let* ([delta (quotient delta (if first? damp 2))]
         [delta (+ delta (quotient delta numpoints))])
    (let loop ([delta delta] [k 0])
      (if (> delta (quotient (* (- base tmin) tmax) 2))
          (loop (quotient delta (- base tmin)) (+ k base))
          (+ k (quotient (* (+ (- base tmin) 1) delta) (+ delta skew)))))))

(define (threshold k bias)
  (cond [(<= k bias) tmin]
        [(>= k (+ bias tmax)) tmax]
        [else (- k bias)]))

(define (digit->char d)
  (integer->char (if (< d 26)
                     (+ d (char->integer #\a))
                     (+ (- d 26) (char->integer #\0)))))

(define (char->digit c)
  (cond [(char<=? #\a c #\z) (- (char->integer c) (char->integer #\a))]
        [(char<=? #\A c #\Z) (- (char->integer c) (char->integer #\A))]
        [(char<=? #\0 c #\9) (+ 26 (- (char->integer c) (char->integer #\0)))]
        [else #f]))

(define (scalar-value? n)
  (and (<= n #x10FFFF) (not (<= #xD800 n #xDFFF))))

(define (punycode-encode str)
  (define cps (map char->integer (string->list str)))
  (define basic-count (count (λ (cp) (< cp initial-n)) cps))
  (define out (open-output-string))
  (for ([cp (in-list cps)] #:when (< cp initial-n))
    (write-char (integer->char cp) out))
  (unless (zero? basic-count) (write-char #\- out))
  (define (write-delta q bias)
    (let loop ([q q] [k base])
      (define t (threshold k bias))
      (cond [(< q t) (write-char (digit->char q) out)]
            [else (write-char (digit->char (+ t (modulo (- q t) (- base t)))) out)
                  (loop (quotient (- q t) (- base t)) (+ k base))])))
  (let loop ([n initial-n] [delta 0] [bias initial-bias] [h basic-count])
    (when (< h (length cps))
      (define m (apply min (filter (λ (cp) (>= cp n)) cps)))
      (define-values (next-delta next-bias next-h)
        (for/fold ([delta (+ delta (* (- m n) (add1 h)))]
                   [bias bias]
                   [h h])
                  ([cp (in-list cps)])
          (cond [(< cp m) (values (add1 delta) bias h)]
                [(= cp m) (write-delta delta bias)
                          (values 0 (adapt delta (add1 h) (= h basic-count)) (add1 h))]
                [else (values delta bias h)])))
      (loop (add1 m) (add1 next-delta) next-bias next-h)))
  (get-output-string out))

(define (read-delta digits bias)
  (let loop ([digits digits] [delta 0] [w 1] [k base])
    (match digits
      ['() (values #f '())]
      [(cons d more)
       (define t (threshold k bias))
       (if (< d t)
           (values (+ delta (* d w)) more)
           (loop more (+ delta (* d w)) (* w (- base t)) (+ k base)))])))

(define (punycode-decode str)
  (define delimiter
    (for/last ([c (in-string str)] [i (in-naturals)] #:when (char=? c #\-)) i))
  (define-values (basic encoded)
    (if (and delimiter (> delimiter 0))
        (values (substring str 0 delimiter) (substring str (add1 delimiter)))
        (values "" str)))
  (define digits (map char->digit (string->list encoded)))
  (and (ascii-string? basic)
       (andmap values digits)
       (let loop ([output (map char->integer (string->list basic))]
                  [digits digits]
                  [n initial-n]
                  [i 0]
                  [bias initial-bias])
         (if (null? digits)
             (list->string (map integer->char output))
             (let-values ([(delta more) (read-delta digits bias)])
               (and delta
                    (let* ([len (add1 (length output))]
                           [next-i (+ i delta)]
                           [next-n (+ n (quotient next-i len))]
                           [pos (modulo next-i len)])
                      (and (scalar-value? next-n)
                           (let-values ([(before after) (split-at output pos)])
                             (loop (append before (list next-n) after)
                                   more
                                   next-n
                                   (add1 pos)
                                   (adapt delta len (zero? i))))))))))))

;; ~~ IDNA2008 tables (RFC 5892) ~~~~~~~~~~~~~~~~~

(define tables (include (lib "splitflap/private/idna-tables.rktd")))

(define idna-unicode-version (hash-ref tables 'unicode-version))
(define property-ranges (hash-ref tables 'properties))
(define bidi-ranges (hash-ref tables 'bidi))
(define joining-ranges (hash-ref tables 'joining))
(define virama-ranges (hash-ref tables 'virama))
(define script-ranges (hash-ref tables 'scripts))

(define (range-ref ranges cp)
  (let loop ([lo 0] [hi (vector-length ranges)])
    (and (< lo hi)
         (let* ([mid (quotient (+ lo hi) 2)]
                [r (vector-ref ranges mid)])
           (cond [(< cp (vector-ref r 0)) (loop lo mid)]
                 [(> cp (vector-ref r 1)) (loop (add1 mid) hi)]
                 [else (vector-ref r 2)])))))

(define (property cp) (or (range-ref property-ranges cp) 'DISALLOWED))
(define (bidi-class cp) (or (range-ref bidi-ranges cp) 'L))
(define (joining-type cp) (range-ref joining-ranges cp))
(define (virama? cp) (and (range-ref virama-ranges cp) #t))
(define (script cp) (range-ref script-ranges cp))

(define (code-point-name cp)
  (format "U+~a ~a" (string-upcase (~r cp #:base 16 #:min-width 4 #:pad-string "0"))
          (integer->char cp)))

;; ~~ Contextual rules (RFC 5892 appendix A) ~~~~~

(define (zwnj-joins? cps i)
  (define (scan j step accepted)
    (and (< -1 j (vector-length cps))
         (let ([type (joining-type (vector-ref cps j))])
           (if (eq? type 'T)
               (scan (+ j step) step accepted)
               (and (memq type accepted) #t)))))
  (and (scan (sub1 i) -1 '(L D))
       (scan (add1 i) 1 '(R D))))

(define (context-rule-ok? cps i)
  (define cp (vector-ref cps i))
  (define before (and (> i 0) (vector-ref cps (sub1 i))))
  (define after (and (< (add1 i) (vector-length cps)) (vector-ref cps (add1 i))))
  (define (label-has? pred) (for/or ([c (in-vector cps)]) (pred c)))
  (cond
    [(= cp #x200C) (or (and before (virama? before)) (zwnj-joins? cps i))]
    [(= cp #x200D) (and before (virama? before))]
    [(= cp #x00B7) (and (eqv? before #x006C) (eqv? after #x006C))]
    [(= cp #x0375) (and after (eq? (script after) 'Greek))]
    [(memv cp '(#x05F3 #x05F4)) (and before (eq? (script before) 'Hebrew))]
    [(= cp #x30FB) (label-has? (λ (c) (memq (script c) '(Hiragana Katakana Han))))]
    [(<= #x0660 cp #x0669) (not (label-has? (λ (c) (<= #x06F0 c #x06F9))))]
    [(<= #x06F0 cp #x06F9) (not (label-has? (λ (c) (<= #x0660 c #x0669))))]
    [else #f]))

;; ~~ Label validation (RFC 5891 section 4.2) ~~~~

(define (u-label-problem label)
  (define cps (for/vector ([c (in-string label)]) (char->integer c)))
  (define (problem message . details) (domain-problem message details))
  (cond
    [(not (string=? label (string-normalize-nfc label)))
     (problem "label is not in Unicode Normalization Form C")]
    [(or (string-prefix? label "-") (string-suffix? label "-"))
     (problem "label may not start or end with a hyphen")]
    [(and (>= (vector-length cps) 4) (= (vector-ref cps 2) 45) (= (vector-ref cps 3) 45))
     (problem "label may not have hyphens in the third and fourth positions")]
    [(memq (char-general-category (string-ref label 0)) '(mn mc me))
     (problem "label may not start with a combining mark")]
    [(for/first ([cp (in-vector cps)]
                 #:unless (memq (property cp) '(PVALID CONTEXTJ CONTEXTO)))
       cp)
     => (λ (cp)
          (problem (if (eq? (property cp) 'UNASSIGNED)
                       (format "label contains a character not assigned in Unicode ~a"
                               idna-unicode-version)
                       "label contains a character that IDNA2008 does not permit")
                   "character" (code-point-name cp)))]
    [(for/first ([cp (in-vector cps)]
                 [i (in-naturals)]
                 #:when (memq (property cp) '(CONTEXTJ CONTEXTO))
                 #:unless (context-rule-ok? cps i))
       cp)
     => (λ (cp)
          (problem "label contains a character that is not permitted in this position"
                   "character" (code-point-name cp)))]
    [else #f]))

(define (add-details p . details)
  (domain-problem (domain-problem-message p) (append details (domain-problem-details p))))

(define (u-label->a-label label)
  (define a-label (string-append "xn--" (punycode-encode label)))
  (cond
    [(u-label-problem label) => (λ (p) (add-details p "label" label))]
    [(> (string-length a-label) 63)
     (domain-problem "label is longer than 63 characters when converted to an A-label"
                     (list "label" label "A-label" a-label))]
    [else a-label]))

(define (a-label->u-label label)
  (define lowercase (string-downcase label))
  (define u-label (punycode-decode (substring lowercase 4)))
  (cond
    [(not u-label)
     (domain-problem "label starts with xn-- but is not valid Punycode" (list "label" label))]
    [(ascii-string? u-label)
     (domain-problem "label starts with xn-- but encodes no non-ASCII characters" (list "label" label))]
    [(u-label-problem u-label) => (λ (p) (add-details p "label" label "U-label" u-label))]
    [(not (string=? lowercase (string-append "xn--" (punycode-encode u-label))))
     (domain-problem "label is not the A-label of its U-label" (list "label" label "U-label" u-label))]
    [else u-label]))

;; ~~ Bidi rule (RFC 5893) ~~~~~~~~~~~~~~~~~~~~~~~

(define (rtl-label? label)
  (and (not (ascii-string? label))
       (for/or ([c (in-string label)])
         (and (memq (bidi-class (char->integer c)) '(R AL AN)) #t))))

(define (bidi-rule-ok? label)
  (define classes (for/list ([c (in-string label)]) (bidi-class (char->integer c))))
  (define last-class (for/last ([c (in-list classes)] #:unless (eq? c 'NSM)) c))
  (define (only allowed) (andmap (λ (c) (memq c allowed)) classes))
  (match classes
    [(cons (or 'R 'AL) _)
     (and (only '(R AL AN EN ES CS ET ON BN NSM))
          (memq last-class '(R AL EN AN))
          (not (and (memq 'EN classes) (memq 'AN classes))))]
    [(cons 'L _)
     (and (only '(L EN ES CS ET ON BN NSM))
          (memq last-class '(L EN)))]
    [_ #f]))

(define (bidi-rule-violation labels)
  (and (ormap rtl-label? labels)
       (findf (λ (label) (not (bidi-rule-ok? label))) labels)))
