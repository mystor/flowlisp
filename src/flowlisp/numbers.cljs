(ns flowlisp.numbers
  [:require [flowlisp.core :refer [definefunction Exception]]])

(defn isNaN? [x]
  (and (number? x) (not= x x)))

(defn definenumerical
  "like definefunction, except all arguments are numbers,
  and the return value throws an error if it is NaN"
  [id cb]
  (definefunction id (fn [& args]
                       (if (every? number? args)
                         (let [value (apply cb args)]
                           (if (isNaN? value)
                             (throw (Exception. "Result is not a number"))
                             value))
                         (throw (Exception. "Expected number, found non-number"))))))

;; Basic arithmetic
(definenumerical '+ +)
(definenumerical '- -)
(definenumerical '* *)
(definenumerical '/ /)

(definenumerical 'quotient quot)
(definenumerical 'remainder rem)
(definenumerical 'modulo mod)

(definenumerical 'abs (fn [x]
                        (if (neg? x) (- x) x)))

(definenumerical 'min min)
(definenumerical 'max max)

(definenumerical 'sqrt #(.sqrt js/Math %))
(definenumerical 'expt #(.pow js/Math %1 %2))
(definenumerical 'exp #(.pow js/Math (.-E js/Math) %))
(definenumerical 'log #(.log js/Math %))
(definenumerical 'floor #(.floor js/Math %))

(definenumerical 'gcd (fn gcd [a b]
                        (if (zero? b)
                          a
                          (recur b (rem a b)))))


;; Comparisons
(definenumerical '> >)
(definenumerical '< <)
(definenumerical '= =)
(definenumerical '<= <=)
(definenumerical '>= >=)
