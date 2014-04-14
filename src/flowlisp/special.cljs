(ns flowlisp.special
  "Special forms may not evaluate all of their arguments, and may modify program state.
  All other functions should be based on core/pure-fn"
  (:require [flowlisp.core :as core :refer [definesyntax
                                            definetransform
                                            definefunction
                                            evaluate
                                            IncompleteException
                                            Exception]]))

(definesyntax 'define
  (fn [state id raw-value]
    (when (not (symbol? id))
      (throw (Exception. "Can only define symbols")))
    (let [value (evaluate state raw-value :value)
          context-id (core/context-id state)
          context (assoc (core/context state) id value)
          state (assoc-in state [:objects context-id] context)
          state (core/return state value)]
      ;; While we are complete, we throw IncompleteException here, as we want to
      ;; directly mutate the state, rather than simply return a value.
      (throw (IncompleteException. state)))))

(definefunction 'begin
  (fn [& args] (last args)))

(definesyntax 'quote
  (fn [state arg]
    arg))

(definesyntax 'lambda
  (fn [state args & body]
    ;; TODO: Ensure that we only have valid args forms
    (core/UserFn. "<Anonymous-Fn>" args body (core/context-id state))))

(definesyntax 'if
  (fn [state condition then else]
    (if (true? (evaluate state condition :condition))
      (evaluate state then :then)
      (evaluate state else :else))))

(definetransform 'and
  (fn [condition & more]
    (if (nil? more)
      `(:identity ~condition)
      `(:if ~condition
         (:and ~@more)
         ~condition))))

(definetransform 'or
  (fn [condition & more]
    (if (nil? more)
      `(:identity ~condition)
      `(:if ~condition
         ~condition
         (:or ~@more)))))

(definetransform 'let
  (fn [[& lets] & body]
    `((:lambda ~(map first lets) ~@body) ~@(map second lets))))

(definetransform 'let*
  (fn [[& lets] & body]
    `((:lambda ()
        ~@(map #(cons :define %) lets)
        ~@body))))

(defn set!-internal
  [state id value context-id]
  (let [context (get (:objects state) context-id)]
    (if (nil? context)
      (assoc-in state [:objects :global id] value)
      (if (core/novalue? (get context id core/novalue))
        (recur state id value (:parent context-id))
        (assoc-in state [:objects context-id id] value)))))

(definesyntax 'set!
  (fn [state id raw-value]
    (when (not (symbol? id))
      (throw (Exception. "Can only set! symbols")))
    (let [value (evaluate state raw-value :value)
          state (set!-internal state id value (core/context-id state))
          state (core/return state value)]
      ;; This is similar to define, see above
      (throw (IncompleteException. state)))))

(definefunction 'identity identity)

(definefunction 'list list)
