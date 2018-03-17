(ns streetgraph.core
  (:gen-class))

(require
  '[streetgraph.csv :as csv]
  '[streetgraph.osm :as osm]
  '[streetgraph.vis :as viz])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (time
    (let [roads (-> osm/ways
                    osm/get-roads
                    osm/filter-roads)]
      (do
        (-> roads
            (viz/render-svg (osm/get-coordinates osm/osm-file)))
        (-> roads
            csv/to-csv)))))