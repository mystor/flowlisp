(ns flowlisp.special
  [:require [flowlisp.core :refer :all]])


;; DEFINE ;;
(defn sp-define
  [state ctx]
  ((chain eval-arg (fn [{[entry] :stack :as state} ctx next]
                     (next (update-in state [:objects (:context entry)]
                                      #(assoc % (second (:source entry)) (:value ctx))) ctx)))
   state (assoc ctx :src-idx 2)))
(register-keyword 'define sp-define)

;; BEGIN ;;
(register-keyword 'begin
                  (pure-fn
                   #(last %&)))

;; QUOTE ;;
(defn sp-quote
  [{[{[_ arg] :source}] :stack :as state} ctx]
  (fl-return state (assoc ctx :value arg)))
(register-keyword 'quote sp-quote)

;; LAMBDA ;;
(defn sp-lambda
  [state ctx]
  (let [entry (stack-entry state)
        rest-src (rest (:source entry))
        args (first rest-src)
        commands (rest rest-src)
        context (:context entry)]
    (if (not (seq? args))
      (throw (Exception. args))
      (fl-return state (assoc ctx :value (user-fn "<Anonymous-Fn>" args commands context))))))
(register-keyword 'lambda sp-lambda)

;; IF ;;
(register-keyword 'if (chain eval-arg
                             (fn fl-if [state ctx next]
                               (if (true? (:value ctx))
                                 (eval-arg state (assoc ctx :src-idx 2) next)
                                 (eval-arg state (assoc ctx :src-idx 3) next)))))

;; AND ;;
(register-keyword 'and (pure-fn (fn [& args] (reduce #(and %1 %2) true args))))

;; OR ;;
(register-keyword 'or (pure-fn (fn [& args] (reduce #(or %1 %2) false args))))

;; LET ;;
(defn sp-let
  [[& lets] & body]
  `((:lambda ~(map first lets)
             ~@body) ~@(map second lets)))
(register-keyword 'let
                  (pure-macro sp-let))

;; SET! ;;
(defn sp-set!-internal
  [{[{[_ k] :source call-context :context}] :stack
    objects :objects :as state}
   {context :context value :value :as ctx} next]
  (cond
   (nil? context)     (recur state (assoc ctx :context call-context) next)
   (novalue? context) (next (assoc-in state [:objects :global k] value) ctx)
   (novalue?
    (get
     (get objects context)
     k novalue))      (recur state (assoc ctx :context (:parent context novalue)) next)
   :else              (next (assoc-in state [:objects context k] value) ctx)))

(defn sp-set!
  [state ctx]
  ((chain eval-arg sp-set!-internal) state (assoc ctx :src-idx 2)))
(register-keyword 'set! sp-set!)
