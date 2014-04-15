(ns the.parsatron)

(defmacro defparser
  "Defines a new parser. Parsers are simply functions that accept the
   5 arguments state, cok, cerr, eok, eerr but this macro takes care
   of writing that ceremony for you and wraps the body in a >>"
  [name args & body]
  `(defn ~name ~args
     (fn [state# cok# cerr# eok# eerr#]
       (let [p# (>> ~@body)]
         (the.parsatron/Continue. #(p# state# cok# cerr# eok# eerr#))))))

(defmacro >>
  "Expands into nested nxt forms"
  ([m] m)
  ([m n] `(the.parsatron/nxt ~m ~n))
  ([m n & ms] `(the.parsatron/nxt ~m (>> ~n ~@ms))))

(defmacro let->>
  "Expands into nested bind forms"
  [[& bindings] & body]
  (let [[bind-form p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(the.parsatron/bind ~p (fn [~bind-form] ~@body))
      `(the.parsatron/bind ~p (fn [~bind-form] (let->> ~(drop 2 bindings) ~@body))))))
