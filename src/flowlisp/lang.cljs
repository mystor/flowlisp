(ns flowlisp.lang
  (:require [flowlisp.core :refer [tick]]
            [flowlisp.special]
            [flowlisp.numbers]))

;; Basic testing functions ;;
(defn initial-state
  [source]
  {:stack (list {:source source
                 :context :global
                 :dest {}}
                {:target :result
                 :dest {}})
   :objects {:global {}}})

(defn run
  [state]
  (if (<= (count (:stack state)) 1)
    (:result (:dest (first (:stack state))))
    (recur (tick state))))

(defn fleval
  [src]
  (run (initial-state src)))

;; WOO TESTING ;;
(.log js/console "TESTING123")
(try
  (.log js/console (fleval '(+ 1 1)))
  (catch flowlisp.core/Exception e
    (.log js/console (:message e))))
(.log js/console "TESTING123-DONE")

(.log js/console "TEST2")
(try
  (.log js/console (fleval '(begin (+ 1 1))))
  (catch flowlisp.core/Exception e
    (.log js/console (:message e))))
(.log js/console "TEST2-DONE")

(.log js/console "TEST3")
(try
  (.log js/console (fleval
   '(begin
     (define x 10)
     (+ x 1))))
  (catch flowlisp.core/Exception e
    (.log js/console (:message e))))
(.log js/console "TEST3-DONE")

(.log js/console "TEST4")
(try
  (fleval
   '(let ((x 10)
          (y 20))
      (+ x y)))
  (catch flowlisp.core/Exception e
    (.log js/console (:message e))))
(.log js/console "TEST4-DONE")

(try
 (fleval
 '(begin
   (define function (lambda (x)
                            (set! x 10)))
   (define x 20)
   (function)
   x))
  (catch flowlisp.core/Exception e
    (.error js/console (:message e))))

(.log js/console "TEST5-DONE")

(fleval
 '(or false false))
(.log js/console "TEST6-DONE")

(fleval '(sqrt 4))
