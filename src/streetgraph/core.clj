(ns streetgraph.core
  (:require
    [streetgraph.osm.saving_backup :refer [serialize-to]]
    [clojure.tools.cli :as cli])
  (:gen-class))

;; hope I'll deal with it later
(def cli-options
  [["-s" "--streets" "path to the city.xml doc"
    :parse-fn #(str %)
    :default nil
    ; :validate \\ just do it! later
    ]
   ["-c" "--coordinates" "user coordinates, latitude and londtitude. unless given, random ones are used "
    :default nil]
   ["-h" "--help"]])

(defn -main
  "zravstvuite"
  [& args]
  (serialize-to "resources"))
