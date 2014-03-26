(ns flowlisp.core)

;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Maybe this should be moved into a utility module
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
(defrecord UserFn [id source context])
(defn user-fn?
  [x]
  (instance? UserFn x))

(defn constant?
  [x]
  (or (number? x) (string? x) (true? x) (false? x)))


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
    (if (nil? context)
      (let [value (get @keywords arg novalue)]
        (if (novalue? value)
          (throw (Exception. "Could not resolve arg"))
          value))
      (let [resolved-context (get (:objects state) context)
            parent (:parent resolved-context)
            value (get resolved-context arg novalue)]
        (if (novalue? value)
          (recur state arg parent)
          value)))))

(defn register-keyword
  [kwd value]
  (swap! keywords #(assoc % kwd value)))

(defn eval-arg
  "Middleware - evaluates the positional argument at :src-idx, and Calls
  next when it is ready"
  [state ctx next]
  (let [idx (:src-idx ctx)
        entry (stack-entry state)
        context (:context entry)
        source (nth (:source entry) idx)
        dest (get (:dest entry) idx novalue)]

    (if (novalue? dest)
      (if (or (constant? source) (symbol? source))
        (recur (update-stack-entry state  ; We can resolve it instantly!
                                   #(assoc-in % [:dest idx]
                                              (fl-resolve state source context)))
               ctx next)
        (-> state  ; We have to perform a call to resolve it, add it to the stack
            (update-stack-entry #(assoc % :target idx))
            (mk-stack-entry source context)))
      (next state  ; It has been resolved, call next
            (-> ctx
                (assoc :src-idx (inc idx))
                (assoc :value dest))))))

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
  (basic-fn (fn [state ctx next args]
              (next state (assoc ctx :value (apply cb args))))))

(defn call-user-fn
  "Call a user defined function (will be executed on the stack)"
  [state ctx]
  (throw (Exception. "Not implemented yet")))

(defn do-action
  "Performs the correct action for the given action (action is (:value ctx))"
  [state ctx next]
  (let [action (:value ctx)]
    (cond
     (fn? action) (action state ctx)
     (user-fn? action) (call-user-fn state ctx)
     :else (throw (Exception. "Cannot call non-callable (first element in form must be callable)")))))

;; On a tick, we evaluate the action, and then perform it
(def do-tick (chain eval-arg do-action))
(defn tick
  "Run one iteration of the program"
  [state]
  (do-tick state {:src-idx 0}))

;; Define the keywords
;; These are added to the keywords atom
;; TODO: Move these to a seperate module? (at least the definitions)

;; Basic math
(register-keyword '+ (pure-fn +))
(register-keyword '- (pure-fn -))
(register-keyword '* (pure-fn *))
(register-keyword '/ (pure-fn /))

;; Comparisons
(register-keyword '> (pure-fn >))
(register-keyword '< (pure-fn <))
(register-keyword '= (pure-fn =))
(register-keyword '<= (pure-fn <=))
(register-keyword '>= (pure-fn >=))

;; basic table operations
; (table a b, c d)
; (list a b c d)  - equivalent to (table 1 a 2 b 3 c 4 d)
; (union a b) - keys of both united as one!

;; if statement
(register-keyword 'if (chain eval-arg
                             (fn fl-if [state ctx next]
                               (if (true? (:value ctx))
                                 (eval-arg state (assoc ctx :src-idx 2) next)
                                 (eval-arg state (assoc ctx :src-idx 3) next)))))

(register-keyword 'do
                  (pure-fn
                   #(last %&)))

;; Testing it!
(def state {:stack (list {:source '(do 1 10 (+ 1 5))
                          :target nil
                          :context nil
                          :dest {}}
                         {:source '()
                          :target :result
                          :context nil
                          :dest {}})
            :objects {}})

(defn run
  [state]
  (if (<= (count (:stack state)) 1)
    (:result (:dest (first (:stack state))))
    (recur (tick state))))

(run state)

(tick state)
(tick (tick state))
(tick (tick (tick state)))
