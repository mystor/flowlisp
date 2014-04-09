(ns flowlisp.core)

(defrecord Exception [message])

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

(defn stack-entry
  "Retrieves the current stack entry"
  [state]
  (first (:stack state)))

(defn update-stack-entry
  "Calls cb, passing the current stack entry. Return values replaces the current stack entry"
  [state cb]
  (update-in state [:stack] #(update-first % cb)))

(defn mk-stack-entry
  "Creates a new stack entry and adds it to the stack"
  [state source context]
  (update-in state [:stack]
             #(cons {:source source
                     :context context
                     :dest {}
                     :target nil} %)))

;; This is an object to represent the non-existance of a value.
;; All instances of NoValue are equal, and will match with = (not identical?)
(defrecord NoValue [])
(def novalue (NoValue.))
(defn novalue?
  "Is the value an instance of NoValue?"
  [x]
  (instance? NoValue x))

;; This is the parent type for a user function
(defrecord UserFn [id args source context])
(defn user-fn
  [id args source context]
  (UserFn. id args source context))
(defn user-fn?
  [x]
  (instance? UserFn x))

(defn constant?
  [x]
  (or (number? x) (string? x) (true? x) (false? x) (nil? x)))

(defn instant?
  [x]
  (or (constant? x) (symbol? x) (keyword? x)))


;;;;;;;;;;;;;;;;;;;;;
;; CORE EVALUATION ;;
;;;;;;;;;;;;;;;;;;;;;

(defn fl-return
  "Returns the value (:value ctx) to the previous entry on the stack
  Pops the current stack entry off of the stack."
  [state ctx]
  (update-in state [:stack] (fn [stack]
                              (let [stack (rest stack)
                                    entry (first stack)]
                                (cons (update-in entry [:dest]
                                                 #(assoc % (:target entry) (:value ctx)))
                                      (rest stack))))))

(defn chain
  "Chains together the middleware stack.
  The top of the stack will be a fl-return"
  [& middleware-list]
  (if (empty? middleware-list)
    fl-return ; The default action is to return
    (let [middleware (first middleware-list)
          next (apply chain (rest middleware-list))]
      (fn [state ctx]
        (middleware state ctx next)))))

(def keywords (atom {}))
(defn fl-resolve
  [state arg context]
  (if (constant? arg)
    arg
    (if (keyword? arg)
      (recur state (symbol (name arg)) nil)
      (if (nil? context)
        (let [value (get @keywords arg novalue)]
          (if (novalue? value)
            (throw (Exception. (str "Could not resolve " (name arg))))
            value))
        (let [resolved-context (get (:objects state) context)
              parent (:parent resolved-context)
              value (get resolved-context arg novalue)]
          (if (novalue? value)
            (recur state arg parent)
            value))))))

(defn register-keyword
  [kwd value]
  (swap! keywords #(assoc % kwd value)))

(defn eval-to
  "Middleware - evaluates the form :src and calls next when it is ready
  Places the resulting item at :target"
  [state ctx source target next]
  (let [entry (stack-entry state)
        context (:context entry)
        value (get (:dest entry) target novalue)]
    (if (novalue? value)
      (if (instant? source)
        (recur (update-stack-entry state  ; We can resolve it instantly
                                   #(assoc-in % [:dest target]
                                              (fl-resolve state source context)))
               ctx source target next)
        (-> state  ; We have to perform a call - add it to the stack
            (update-stack-entry #(assoc % :target target))
            (mk-stack-entry source context)))
      (next state  ; We have resolved this already - call next
            (assoc ctx :value value)))))

(defn eval-arg
  [state ctx next]
  (let [idx (:src-idx ctx)]
    (eval-to state (assoc ctx :src-idx (inc idx)) (nth (:source (stack-entry state)) idx) idx next)))

(defn basic-fn
  "Generates a middleware stack which will evaluate all arguments, and then
  call cb when the arguments have all been evaluated.

  cb will be called with the following arguments: [state ctx next args]"
  [cb]
  (fn [state ctx]
    (let [entry (stack-entry state)
          args (rest (:source entry))
          arity (count args)]
      ((apply chain
             (concat
              (repeat arity eval-arg)
              [(fn [state ctx next]
                 (let [entry (stack-entry state)
                       dest (:dest entry)
                       args (map #(get dest %) (range 1 (+ arity 1)))]
                   (cb state ctx next args)))])) state ctx))))

(defn pure-fn
  "A pure function evaluates all of its arguments.
  It does not modify state, and only depends on its arguments.
  It is called with the arguments as its arguments"
  [cb]
  (with-meta (basic-fn (fn [state ctx next args]
                         (next state (assoc ctx :value (apply cb args)))))
    {:procedure true}))

(defn basic-macro
  [cb]
  (fn [state ctx]
    (let [entry (stack-entry state)
          args (rest (:source entry))]
      (cb state ctx next args))))

(defn pure-macro
  [cb]
  (basic-macro (fn [state ctx next args]
                 (mk-stack-entry (update-in state [:stack] rest)
                                 (apply cb args)
                                 (:context (stack-entry state))))))

(defn gen-context
  [action args]
  (-> (zipmap (:args action) (concat args (repeat nil)))
      (assoc :parent (:context action))))

(defn call-user-fn
  "Call a user defined function (will be executed on the stack)"
  [state ctx]
  (let [action (:value ctx)
        source (cons :begin (:source action))
        src-key (random-str 10)
        handler (fn [state ctx next args]
                  (-> state
                      (update-in [:stack] rest)
                      (update-in [:objects] #(assoc % src-key (gen-context action args)))
                      (mk-stack-entry source src-key)))]
    ((basic-fn handler) state ctx)))

(defn do-action
  "Performs the correct action for the given action (action is (:value ctx))"
  [state ctx next]
  (let [action (:value ctx)]
    (cond
     (fn? action) (action state ctx)
     (user-fn? action) (call-user-fn state ctx)
     :else (throw (Exception. (str "Cannot call non-callable (first element in form must be callable)" action))))))

;; On a tick, we evaluate the action, and then perform it
(def do-tick (chain eval-arg do-action))
(defn tick
  "Run one iteration of the program"
  [state]
  (do-tick state {:src-idx 0}))

;; Define the keywords
;; These are added to the keywords atom
;; TODO: Move these to a seperate module? (at least the definitions)
