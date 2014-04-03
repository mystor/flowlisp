(ns flowlisp.special
  "Special forms may not evaluate all of their arguments, and may modify program state.
  All other functions should be based on core/pure-fn"
  [:require [flowlisp.core :as core]])


;; DEFINE ;;
(defn sp-define
  [state ctx]
  ((core/chain core/eval-arg (fn [{[entry] :stack :as state} ctx next]
                               (next (update-in state [:objects (:context entry)]
                                                #(assoc % (second (:source entry)) (:value ctx))) ctx)))
   state (assoc ctx :src-idx 2)))
(core/register-keyword 'define sp-define)

;; BEGIN ;;
(core/register-keyword 'begin
                       (with-meta (core/pure-fn
                                   #(last %&))
                         {:procedure false}))

;; QUOTE ;;
(defn sp-quote
  [{[{[_ arg] :source}] :stack :as state} ctx]
  (core/fl-return state (assoc ctx :value arg)))
(core/register-keyword 'quote sp-quote)

;; LAMBDA ;;
(defn sp-lambda
  [state ctx]
  (let [entry (core/stack-entry state)
        rest-src (rest (:source entry))
        args (first rest-src)
        commands (rest rest-src)
        context (:context entry)]
    (if (not (seq? args))
      (throw (core/Exception. args))
      (core/fl-return state (assoc ctx :value (core/user-fn "<Anonymous-Fn>" args commands context))))))
(core/register-keyword 'lambda sp-lambda)

;; IF ;;
(core/register-keyword 'if (core/chain core/eval-arg
                                       (fn fl-if [state ctx next]
                                         (if (true? (:value ctx))
                                           (core/eval-arg state (assoc ctx :src-idx 2) next)
                                           (core/eval-arg state (assoc ctx :src-idx 3) next)))))

;; AND ;;
(defn sp-and
  [e & more]
  `(:if ~e
     ~(if (empty? more) true `(:and ~@more))
     false))
(core/register-keyword 'and (core/pure-macro sp-and))

;; OR ;;
(defn sp-or
  [e & more]
  `(:if ~e
     true
     ~(if (empty? more) false `(:or ~@more))))
(core/register-keyword 'or (core/pure-macro sp-or))

;; LET ;;
(defn sp-let
  [[& lets] & body]
  `((:lambda ~(map first lets)
             ~@body) ~@(map second lets)))
(core/register-keyword 'let
                       (core/pure-macro sp-let))

;; SET! ;;
(defn sp-set!-internal
  [{[{[_ k] :source call-context :context}] :stack
    objects :objects :as state}
   {context :context value :value :as ctx} next]
  (cond
   (nil? context)     (recur state (assoc ctx :context call-context) next)
   (core/novalue? context) (next (assoc-in state [:objects :global k] value) ctx)
   (core/novalue?
    (get
     (get objects context)
     k core/novalue))      (recur state (assoc ctx :context (:parent context core/novalue)) next)
   :else              (next (assoc-in state [:objects context k] value) ctx)))

(defn sp-set!
  [state ctx]
  ((core/chain core/eval-arg sp-set!-internal) state (assoc ctx :src-idx 2)))
(core/register-keyword 'set! sp-set!)
