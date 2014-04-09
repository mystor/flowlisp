(ns flowlisp.reader)

;; XXX: This will probably break in cljs - consider writing language agnostic ;;
(defn string->number
  "Parses"
  [s]
  (js/parseFloat s))

(def atom-end #{\( \) \" \' \space \tab \newline})
(def whitespace #{\space \tab \newline})
(def digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn parse-symbol
  [s]
  (if (symbol? s)
    (cond
     (contains? digits (first (name s))) (string->number (name s))
     (and (= \- (first (name s))) (contains? digits (second (name s)))) (string->number (name s))
     (= \# (first (name s))) (= \t (second (name s)))
     :else s)
    s))

(defn pop-in
  [stack]
  (let [entering (parse-symbol (last stack))
        stack (into [] (butlast stack))
        newstack (update-in stack [(- (count stack) 1)] #(conj % entering))]
    (if (= :quote (first (last newstack)))
      (pop-in newstack)
      newstack)))

(defn parse-sexpr
  [[c & remainder :as sexp] raw-linum stack]
  (let [linum (if (= \newline c) (inc raw-linum) raw-linum)]
    (if (empty? sexp)
      (first stack)
      (cond
       (vector? (last stack))
         (cond
          (= \( c) (recur remainder linum (conj stack ^{:linum linum} []))
          (= \) c) (recur remainder linum (pop-in stack))
          (= \" c) (recur remainder linum (conj stack ""))
          (= \' c) (recur remainder linum (conj stack ^{:linum linum} [:quote]))
          (contains? whitespace c) (recur remainder linum stack)
          :else (recur remainder linum (conj stack (with-meta (symbol (str c)) {:linum linum}))))
       (string? (last stack))
         (cond
          (= \" c) (recur remainder linum (pop-in stack))
          :else (recur remainder linum (update-in stack [(- (count stack) 1)] #(str % c))))
       (symbol? (last stack))
         (cond
          (contains? atom-end c) (recur sexp raw-linum (pop-in stack))
          :else (recur remainder linum (update-in stack [(- (count stack) 1)]
                                                  #(with-meta (symbol (str (name %) c))
                                                     {:linum linum}))))))))

(defn deep-into-seq
  [v]
  (with-meta (seq (map #(if (vector? %) (deep-into-seq %) %) v)) (meta v)))

(defn lex-parse
  [source]
  (deep-into-seq (cons :begin (parse-sexpr source 1 [[]]))))



(lex-parse "(set!\n'(x y z) 10)\n\n(set! x 5) (#t #f 10)")
