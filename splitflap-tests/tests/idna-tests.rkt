#lang racket/base

(require racket/match
         rackunit
         splitflap/private/idna)

;; ~~ Punycode (RFC 3492) ~~~~~~~~~~~~~~~~~~~~~~~~

(define rfc3492-samples
  '(("ليهمابتكلموشعربي؟"
     "egbpdaj6bu4bxfgehfvwxn")
    ("他们为什么不说中文"
     "ihqwcrb4cv8a8dqg056pqjye")
    ("他們爲什麽不說中文"
     "ihqwctvzc91f659drss3x8bo0yb")
    ("Pročprostěnemluvíčesky"
     "Proprostnemluvesky-uyb24dma41a")
    ("למההםפשוטלאמדבריםעברית"
     "4dbcagdahymbxekheh6e0a7fei0b")
    ("यहलोगहिन्दीक्योंनहींबोलसकतेहैं"
     "i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd")
    ("なぜみんな日本語を話してくれないのか"
     "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa")
    ("세계의모든사람들이한국어를이해한다면얼마나좋을까"
     "989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5jpsd879ccm6fea98c")
    ("почемужеонинеговорятпорусски"
     "b1abfaaepdrnnbgefbadotcwatmq2g4l")
    ("PorquénopuedensimplementehablarenEspañol"
     "PorqunopuedensimplementehablarenEspaol-fmd56a")
    ("TạisaohọkhôngthểchỉnóitiếngViệt"
     "TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g")
    ("3年B組金八先生"
     "3B-ww4c5e180e575a65lsy2b")
    ("安室奈美恵-with-SUPER-MONKEYS"
     "-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n")
    ("Hello-Another-Way-それぞれの場所"
     "Hello-Another-Way--fc4qua05auwb3674vfr0b")
    ("ひとつ屋根の下2"
     "2-u9tlzr9756bt3uc0v")
    ("MajiでKoiする5秒前"
     "MajiKoi5-783gue6qz075azm5e")
    ("パフィーdeルンバ"
     "de-jg4avhby1noc0d")
    ("そのスピードで"
     "d9juau41awczczp")
    ("-> $1.00 <-"
     "-> $1.00 <--")))

(for ([sample (in-list rfc3492-samples)])
  (match-define (list unicode encoded) sample)
  (check-equal? (punycode-encode unicode) encoded)
  (check-equal? (punycode-decode encoded) unicode))

(check-equal? (punycode-decode "b1abfaaepdrnnbgefbaDotcwatmq2g4l")
              (punycode-decode "b1abfaaepdrnnbgefbadotcwatmq2g4l"))

(check-false (punycode-decode "-abc"))
(check-false (punycode-decode "ab!c"))
(check-false (punycode-decode "99999999999"))
