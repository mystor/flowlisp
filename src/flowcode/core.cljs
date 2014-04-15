(ns flowcode.core
  (:require [flowlisp.reader :refer [lex-parse]]
            [flowlisp.core :refer [tick]]
            [flowlisp.lang :refer [initial-state]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            ;[c2.core :refer [unify]]
            ;[c2.dom :refer [replace!]]
            ;[c2.scale :as scale]
            ))

;; The ace editor which contains the user's code.
(def editor (atom nil))

;; editor.session.addGutterDecoration(linum-1, className)
;;


(comment
(.require js/ace #js ["ace/range"]
  (fn [arange]
    (let [Range (.-Range arange)
          session (.getSession @editor)]
      (.addMarker session (Range. 1 0 1 1) "result" "result" true))))
)

(def tick-timeout (atom nil))

;; The om application state
(def app-state (atom {:history (list) :scroll-y 0 :error nil}))

(defn single-tick!
  "Run a single tick, with the most recent state, and "
  []
  (let [history (:history @app-state)
        state (tick (first history))
        cont (> (count (:stack state)) 1)]
    (swap! app-state assoc :history (cons state history))
    cont))

(defn run-ticks! []
  (try
    (loop [i 100]
      (let [cont (single-tick!)]
        (if (and cont (> i 0))
          (recur (dec i))
          (if cont
            (reset! tick-timeout (js/setTimeout run-ticks! 0))
            (do
              (.log js/console (clj->js @app-state))
              (.log js/console (-> @app-state
                                   :history
                                   first
                                   :stack
                                   first
                                   :dest
                                   :result)))))))
    (catch js/Object err
      ;; TODO: Nicer way to do error reporting
      ;; (swap! app-state assoc :history (cons {:hint [:error err]} (:history @app-state)))
      (swap! app-state assoc :error err)
      (.error js/console err))))

(defn on-change
  [e]
  (try
    ;; TODO: Compare to previous initial state, they may be the same!
    (swap! app-state assoc :history (list (initial-state (lex-parse (.getValue @editor)))))
    (swap! app-state assoc :error nil)
    (js/clearTimeout @tick-timeout)
    (run-ticks!) ; run-ticks! should not throw an error, as it should be caught internally
    (catch js/Object err
      (swap! app-state assoc :error err))))

(defn on-scroll
  [e]
  (swap! app-state assoc :scroll-y e))

(defn on-cursor-move
  [e]
  (.log js/console (.getCursor (.-selection @editor))))

(defn code-view
  [data owner]
  (reify
    om/IDidMount
    (did-mount [this]
      ;; The code-view should only render once, when it mounts we attach the
      ;; ace editor to the dom element.
      (reset! editor (.edit js/ace "code"))
      ;; monokai is pretty
      (.setTheme @editor "ace/theme/monokai")
      ;; We are writing scheme!
      (.setMode (.getSession @editor) "ace/mode/scheme")
      (.setOption @editor "scrollPastEnd" true)
      ;; Attach some event listeners
      (.on @editor "change" #(on-change %))
      (.on (.-session @editor) "changeScrollTop" #(on-scroll %))
      (.on (.-selection (.getSession @editor)) "changeCursor" #(on-cursor-move %)))
    om/IRender
    (render [this]
      (dom/div #js {:id "code"}
        "  (+ 1 1\n)"))))

(defn error-display
  [error owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:id "error"}
        (if (not (nil? error))
          (dom/div nil (:message error))
          "")))))

(defn graph
  [data owner]
  (reify
    om/IRender
    (render [this] (dom/span nil))))

(defn flowcode-app
  [app owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [comm]}]
      (dom/div nil
        (om/build code-view app)
        (om/build error-display (:error app))
        (om/build graph app)))))

(om/root
 flowcode-app
 app-state
 {:target (. js/document (getElementById "container"))})
