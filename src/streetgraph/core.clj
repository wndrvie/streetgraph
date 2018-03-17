(ns streetgraph.core
  (:gen-class))
(require
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[streetgraph.osm-processing :as osm]
         '[streetgraph.visualisation :as viz])

(defn to-csv
  [roads]
  (let [adjacency-list (atom {})]
    (do
      (doseq [road roads]
        (doseq [i (range (- (count (:nodes road)) 1))]
          (let [ith (nth (:nodes road) i)
                i+1th (nth (:nodes road) (+ i 1))
                ith-adjccs (ith @adjacency-list)
                i+1th-adjcs (i+1th @adjacency-list)]
            (case (:direction road)
              :--> (if (nil? ith-adjccs)
                     (swap! adjacency-list
                            assoc ith [i+1th])
                     (swap! adjacency-list
                            assoc ith (conj ith-adjccs i+1th)))
              :<-- (if (nil? i+1th-adjcs)
                     (swap! adjacency-list
                            assoc i+1th [ith])
                     (swap! adjacency-list
                            assoc i+1th (conj ith-adjccs ith)))
              :<-> (do
                     (if (nil? ith-adjccs)
                       (swap! adjacency-list
                              assoc ith [i+1th])
                       (swap! adjacency-list
                              assoc ith (conj ith-adjccs i+1th)))
                     (if (nil? i+1th-adjcs)
                       (swap! adjacency-list
                              assoc i+1th [ith])
                       (swap! adjacency-list
                              assoc i+1th (conj ith-adjccs ith))))))))
      (with-open [writer (io/writer "adjacency-list.csv")]
        (csv/write-csv writer
                       (->> @adjacency-list
                            (apply list)
                            (map flatten)))))))

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
            to-csv)))))