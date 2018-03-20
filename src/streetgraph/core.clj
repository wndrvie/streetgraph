(ns streetgraph.core
  (:gen-class))

(require
  '[streetgraph.csv :as csv]
  '[streetgraph.osm :as osm]
  '[streetgraph.vis :as viz])

(defn -main
  "What else to say?"
  [& args]
  (time
    (let [roads (-> osm/ways
                    osm/get-roads
                    osm/filter-roads)]
      (do
        (-> roads
            (viz/render-svg (osm/get-coordinates osm/osm-file)))
        (-> roads
            csv/build-adj-list
            csv/write-csv)))))
