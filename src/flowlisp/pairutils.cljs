(ns flowlisp.pairutils)

(defrecord DottedList [head tail])

(defn dotted?
  [x]
  (instance? DottedList x))

(defn car
  [x]
  (if (dotted? x)
    (car (:head x))
    (first x)))

(defn cdr
  [x]
  (if (dotted? x)
    (if (<= (count (:head x)) 1)
      (:tail x)
      (DottedList. (cdr (:head x)) (:tail x)))
    (rest x)))

(defn pcons
  [a b]
  (cond
   (seq? b) (cons a b)
   (dotted? b) (DottedList. (pcons a (:head b)) (:tail b))
   (seq? a) (DottedList. a b)
   :else (DottedList. (list a) b)))
