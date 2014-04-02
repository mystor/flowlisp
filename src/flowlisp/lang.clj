(ns flowlisp.lang
  [:require [flowlisp.core :refer :all]
            [flowlisp.special :refer :all]])

;; Basic testing functions ;;
(defn initial-state
  [source]
  {:stack (list {:source source
                 :context :global
                 :dest {}}
                {:target :result
                 :dest {}})
   :objects {:global {'x 10}}})

(defn run
  [state]
  (if (<= (count (:stack state)) 1)
    (:result (:dest (first (:stack state))))
    (recur (tick state))))

(defn fleval
  [src]
  (run (initial-state src)))

;; WOO TESTING ;;
(fleval
 '(begin
   (define x 10)
   (+ x 1)
   (quote (+ 1 1 1))))

(fleval
 '(let ((x 10)
        (y 20))
    (+ x y)))

(fleval
 '(begin
   (define function (lambda (x)
                            (set! x 10)))
   (define x 20)
   (function nil)
   x))
