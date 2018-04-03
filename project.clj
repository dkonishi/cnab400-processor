(defproject cnab400_processor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clj-time "0.14.2"]
                 [cheshire "5.8.0"]
                 [dk.ative/docjure "1.12.0"]]
  :main ^:skip-aot cnab400-processor.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
