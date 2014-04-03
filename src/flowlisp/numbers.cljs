(ns flowlisp.numbers
  [:require [flowlisp.core :refer [register-keyword pure-fn]]])

(defn isNaN? [x]
  (and (number? x) (not= x x)))

(defn numerical-fn
  [cb]
  (pure-fn (fn [& args]
             (if (every? number? args)
               (let [value (apply cb args)]
                 (if (isNaN? value)
                   (throw (flowlisp.core/Exception. "Result is not a number"))
                   value))
               (throw (flowlisp.core/Exception. "Expected number, found non-number"))))))

;; Basic math
(register-keyword '+ (pure-fn +))
(register-keyword '- (pure-fn -))
(register-keyword '* (pure-fn *))
(register-keyword '/ (pure-fn /))

;; division stuff
(register-keyword 'quotient (pure-fn quot))
(register-keyword 'remainder (pure-fn rem))
(register-keyword 'modulo (pure-fn mod))

(defn abs [x]
  (cond
   (not (number? x)) (throw (flowlisp.core/Exception. "abs can only be called on a number"))
   (neg? x) (- x)
   :else x))
(register-keyword 'abs (pure-fn abs))


;; Minimum & Maximum value
(register-keyword 'min (pure-fn min))
(register-keyword 'max (pure-fn max))

;; Important functions
;! XXX: Clojurescript specific, consider more language agnostic
(register-keyword 'sqrt (pure-fn #(.sqrt js/Math %)))

(register-keyword 'expt (pure-fn #(.pow js/Math %1 %2)))
(register-keyword 'exp (pure-fn #(.pow js/Math (.-E js/Math) %)))
(register-keyword 'log (pure-fn #(.log js/Math %)))

(defn gcd [a b]
  (cond
   (not (and (number? a) (number? b))) (throw (flowlisp.core/Exception. "gcd must be called on a number"))
   (zero? b) a
   :else (recur b (rem a b))))
(register-keyword 'gcd (pure-fn gcd))

(register-keyword 'floor (pure-fn #(.floor js/Math %)))


;; Comparisons
(register-keyword '> (pure-fn >))
(register-keyword '< (pure-fn <))
(register-keyword '= (pure-fn =))
(register-keyword '<= (pure-fn <=))
(register-keyword '>= (pure-fn >=))