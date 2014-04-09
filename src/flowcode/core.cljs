(ns flowcode.core
  (:require [flowlisp.reader :refer [lex-parse]]
            [flowlisp.core :refer [tick]]
            [flowlisp.lang :refer [initial-state]]
            [c2.core :refer [unify]]
            [c2.dom :refer [replace!]]
            [c2.scale :as scale]))

(defn plot-history
  [history]
  [:section#graph
   [:div#points
    (unify (zipmap (range) history)
           (fn [[i linum]]
             [:div {:style {:background-color "gray"
                            :position "absolute"
                            :width "5px"
                            :height "16px"
                            :top (str (* linum 16) "px")
                            :left (str (* i 5) "px")}}]))]])

(defn plot-history! []
  nil)


(def editor (.edit js/ace "code"))

(def tick-timeout (atom nil))
(def history (atom (list)))

(defn do-tick []
  (try
    (swap! history (fn [history]
                     (conj history (tick (first history)))))
    (if (<= (count (:stack (first @history))) 1)
      (do
        (.log js/console (:result (:dest (first (:stack (first @history))))))
        false)
      true)
    (catch js/Object err
      (.log js/console err)
      false)))

(defn on-tick []
  (loop [i 100]
    (let [cont (do-tick)]
      (if (and cont (> i 0))
        (recur (dec i))
        (do
          (when cont
            (reset! tick-timeout (js/setTimeout on-tick 0)))
          (plot-history!))))))

(defn on-change [e]
  (reset! history (list (initial-state (lex-parse (.getValue editor)))))
  (js/clearTimeout @tick-timeout)
  (try (on-tick) (catch js/Object err (.log js/console err))))

(defn on-scroll [e]
  (.log js/console e))

(defn initialize-editor! []
  (.setTheme editor "ace/theme/monokai")
  (.setMode (.getSession editor) "ace/mode/scheme")
  (.on editor "change" on-change)
  (.on (.-session editor) "changeScrollTop" on-scroll))

(initialize-editor!)

(defn with-unit
  "Wrap a scale with a decorator that appends a unit."
  [scale unit]
  (fn [x]
    (str (scale x) (name unit))))
