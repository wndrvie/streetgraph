(defproject streetgraph "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [dali "0.7.4"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [com.taoensso/nippy "2.13.0"]]
  :main ^:skip-aot streetgraph.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
