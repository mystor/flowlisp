(ns flowlisp.reader
  (:require [the.parsatron :as p :include-macros true :refer [defparser]]
            [flowlisp.pairutils :as pairutils :refer [DottedList]]))

;; Parsatron doesn't have every helper you could want, these are some useful ones
(defparser one-of [string]
  (p/token (fn [c] (some #(= c %) string))))

(defparser seperated-by [item sep]
  (p/let->> [fst (item)
             rst (p/many (p/attempt (p/nxt (sep) (item))))]
    (p/always (cons fst rst))))

(defparser ended-by [item sep]
  (p/many1
   (p/let->> [i (item)
              _ (sep)]
     (p/always i))))

;; Basic tokens
(defparser parse-symbol []
  (one-of "!#$%&|*+-/:<=>?@^_~"))

(def space " \t\n")
(defparser spaces []
  (p/many1
   (one-of space)))

;; Strings
(defparser parse-string []
  (p/char \")
  (p/let->> [string (p/many
                     (p/either (p/attempt (p/nxt (p/char \\) (p/char \"))) ; escaped "
                               (p/token #(not (= % \")))))]    ; anything else
    (p/char \")
    (p/always (apply str string))))

;; Atoms
(defparser parse-atom []
  (p/let->> [first (p/either (p/letter) (parse-symbol))
             rest (p/many (p/choice (p/letter) (parse-symbol) (p/digit)))]
    (p/always (let [s (apply str first rest)]
                (case s
                  "#t" true
                  "#f" false
                  (symbol s))))))

;; Numbers
(defparser positive-number []
  (p/let->> [fst (p/digit)
             cs (p/many (p/either (p/digit) (p/char \.)))]
    (p/always (let [s (apply str fst cs)]
                (js/parseFloat s 10)))))

(defparser negative-number []
  (p/char \-)
  (p/let->> [n (positive-number)]
    (p/always (- n))))

(defparser parse-number []
  (p/either (negative-number) (positive-number)))

;; Lists
(declare parse-expr)

(defparser parse-list []
  (seperated-by parse-expr spaces))

(defparser parse-dotted-list []
  (p/let->> [head (ended-by parse-expr spaces)
             _ (p/char \.)
             _ (spaces)
             tail (parse-expr)]
    (p/always (DottedList. head tail))))

(defparser parse-any-list []
  (p/let->> [start-pos (p/extract #(:pos %))
             _ (p/char \()
             lst (p/either (p/attempt (parse-dotted-list)) (parse-list))
             end-pos (p/extract #(:pos %))
             _ (p/char \))]
    (p/always (with-meta lst {:start start-pos :end end-pos}))))

;; Prefixes
(defparser parse-prefixed [c action]
  (p/let->> [start-pos (p/extract #(:pos %))
             _ (p/char c)
             expr (parse-expr)]
    (p/always (with-meta (list action expr)
                {:start start-pos
                 :end (:end (meta expr))}))))

;; General Parser
(defparser parse-expr []
  (p/choice
   (parse-atom)
   (parse-string)
   (parse-number)
   (parse-prefixed \' 'quote)
   (parse-prefixed \` 'quasiquote)
   (parse-prefixed \, 'unquote)
   (parse-any-list)))

(defparser parse-program []
  (p/either (p/attempt (spaces)) (p/always nil))
  (p/let->> [prog (p/many (p/let->> [l (parse-expr)
                                     _ (spaces)]
                            (p/always l)))]
    (p/eof)
    (p/always prog)))

(defn lex-parse [string]
  (cons :begin (p/run (parse-program) (str string " "))))

(lex-parse "(asd f)")

(p/run (parse-expr) "(asd . f)")
(p/run (parse-expr) "`(asd f (+ ,x 1 10))")
(p/run (parse-expr) "'(asd f (+ x 1))")
(meta (p/run (parse-expr) "'(asd f (+ x 1))"))

(p/run (parse-list) "asd f ghjas")

(p/run (parse-atom) "mea99n$$ie")

(p/run (parse-number) "1")

(p/run (parse-string) "\"Happy Days \\Here! \\\" asd\"")
