(defproject flowlisp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojurescript "0.0-2173"]]
  :plugins [[lein-cljsbuild "1.0.1"]]
  :cljsbuild {
    :builds [{
        :source-paths ["src"]
        :compiler {
          :output-to "out/flowlisp.js"
          :output-dir "out/target"
          :source-map "out/flowlisp.js.map"
          :optimizations :whitespace
          :pretty-print true}}]})
