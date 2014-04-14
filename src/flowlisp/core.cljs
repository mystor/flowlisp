(ns flowlisp.core)

(defrecord Exception [message])
(defrecord IncompleteException [state])
(def keywords (atom {}))

;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Maybe this should be moved into a utility module
(def valid-chars
  (map char (concat (range 48 58) ; 0-9
                    (range 66 91) ; A-Z
                    (range 97 123)))) ; a-z

(defn random-char
  "Generates a random character from 0-9A-Za-z"
  []
  (rand-nth valid-chars))

(defn random-str
  "Generates a random string with length length"
  [length]
  (apply str (take length (repeatedly random-char))))

(defn update-first
  "Calls cb on the first entry of the passed in seq.
  Returns a new seq with the return value as the first entry"
  [lst cb]
  (cons (cb (first lst)) (rest lst)))

(defn update-stack-entry
  "Calls cb, passing the current stack entry. Return values replaces the current stack entry"
  [state cb]
  (update-in state [:stack] #(update-first % cb)))

;; This is an object to represent the non-existance of a value.
;; All instances of NoValue are equal, and will match with = (not identical?)
(defrecord NoValue [])
(def novalue (NoValue.))
(defn novalue?
  "Is the value an instance of NoValue?"
  [x]
  (instance? NoValue x))

;; This is the parent type for a user function

;;;;;;;;;;;;;;;;;;;;;
;; CORE EVALUATION ;;
;;;;;;;;;;;;;;;;;;;;;

(defn raw-args
  "Returns the unevaluated arguments for the current stack entry"
  [state]
  (-> state (:stack) (first) (:source) (rest)))

(defn- cached-args
  "Returns the cached evaluated arguments for the current stack entry"
  [state]
  (-> state (:stack) (first) (:dest)))

;; These functions are for modifying the state object such that something
;; other than the current stack entry is being modified.
;; They do not directly throw IncompleteException.

(defn return
  "Returns a new state, with the top item removed, and value inserted into the next entry"
  [state value]
  (update-in state [:stack]
             (fn [stack]
               (-> stack
                   (rest)
                   (update-first (fn [entry]
                                   (assoc-in entry [:dest (:target entry)] value)))))))

(defn call
  "Returns a new state, with src added to the top of the stack"
  [state src context]
  (update-in state [:stack]
             (fn [stack]
               (cons {:source src
                      :target nil
                      :dest {}
                      :context context} stack))))

(defn swap
  "Returns a new state, with src replacing the top of the stack"
  [state src context]
  (call (update-in state [:stack] rest) src context))

;; evaluate internals ;;

(defn- evaluated-value
  "The cached value of the item with the id"
  [state id & [default]]
  (get (cached-args state) id default))

(defn- evaluated?
  "Is there a cached value with this id in the current state?"
  [state id]
  (not (novalue? (evaluated-value state id novalue))))

(defn- followable?
  "Can the item be followed, or must it be evaluated first?"
  [src]
  (not (list? src)))

(defn context-id
  "Get the identifier for the current context"
  [state]
  (-> state (:stack) (first) (:context)))

(defn context
  "Get the current context"
  [state]
  (get (:objects state) (context-id state)))

(defn- context-lookup
  "Lookup a symbol in the passed in context"
  [src context]
  (get context src novalue))

(defn- keyword-lookup
  "Lookup a keyword in the keywords map"
  [src]
  (get @keywords (name src) novalue))

(defn- follow-symbol
  "Follow a followable symbol"
  [state src context]
  (if (nil? context)
    (let [value (keyword-lookup src)]
      (if (novalue? value)
        (throw (Exception. (str "Cannot resolve symbol: " (name src))))
        value))
    (let [value (context-lookup src context)]
      (if (novalue? value)
        (recur state src (get (:objects state) (:parent context)))
        value))))

(defn- follow
  "Follow a followable symbol, keyword or constant"
  [state src]
  (cond
   (symbol? src) (follow-symbol state src (context state))
   (keyword? src) (let [value (keyword-lookup src)]
                    (if (novalue? value)
                      (throw (Exception. (str "Cannot resolve keyword: " (name src))))
                      value))
   :else src))

(defn- cache-value
  "Cache the value under the id in the current stack entry"
  [state id value]
  (update-stack-entry state
                      (fn [entry]
                        (assoc-in entry [:dest id] value))))

(defn- set-target
  "Set the target value of the current stack entry"
  [state id]
  (update-stack-entry state #(assoc % :target id)))

(defn evaluate
  "Evaluate src in the context of the current state - cache the value with the id id"
  [state src id]
  (cond
   (evaluated? state id) (evaluated-value state id)
   (followable? src) (throw (IncompleteException. (cache-value state id (follow state src))))
   :else (throw (IncompleteException. (call (set-target state id) src (context-id state))))))

;; definesyntax and definefunction are used to define internal functions & syntax.
;; Both will create an entry in the keywords map.
(defn setsymbol [id value]
  (swap! keywords #(assoc % (name id) value)))

(defn definesyntax
  "Define an action which returns a value, cb accepts [state & raw-args]"
  [id cb]
  (setsymbol id (fn [state]
                  (return state (apply cb (cons state (raw-args state)))))))

(defn definetransform
  "Define an action which returns a new form, cb accepts [& raw-args]"
  [id cb]
  (setsymbol id (fn [state]
                  (swap state (apply cb (raw-args state)) (context state)))))

(defn definefunction
  "Define a function, the cb accepts [& args]"
  [id cb]
  (definesyntax id (fn [state & raw-args]
                     (apply cb (map #(evaluate state (second %) (first %)) (zipmap (range) raw-args))))))

;; The base type for user define functions. Generally not used directly, but
;; instead used by the handler code for the (lambda) special form.
;; Implements IFn such that it can be called in the same way as internal functions.
(defrecord UserFn [id args src context]
  IFn
  (invoke [_ state]
    (let [src (cons :begin src)
          context-id (random-str 10)
          argv (map #(evaluate state (second %) (first %)) (zipmap (range) (raw-args state)))
          fn-context (zipmap args (concat argv (repeat '())))
          fn-context (assoc fn-context :parent context)
          state (swap state src context-id)
          state (assoc-in state [:objects context-id] fn-context)]
      (throw (IncompleteException. state)))))

(defn userfn?
  "Is the object x a UserFn?"
  [x]
  (instance? UserFn x))

(defn tick
  "Run a single tick of stack updates"
  [state]
  (try
    (let [entry (-> state (:stack) (first))
          action (evaluate state (first (:source entry)) :action)]
      (action state))

    ;; IncompleteException is thrown when additional calls to tick are required before
    ;; the action is able to complete.  It is thrown very frequently.
    ;; TODO: Investigate the performance implications of using exceptions, and consider
    ;; replacing exception use with something equally readable but not as slow
    (catch IncompleteException e
      (:state e))))
