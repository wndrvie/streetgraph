(ns streetgraph.csv)
;; CSV MANAGING

;(require '[clojure.data.csv :as csv]
;         '[clojure.java.io :as io])
;
;(defn build-adj-list
;  "Takes roads and builds adjacency list.
;   Return fmt: { & :node-id [ vector of adjacent nodes ] }"
;  [roads]
;  (do
;    (println "Building csv...")
;    (apply merge-with into
;           (apply concat
;                  (for [road roads]
;                    (for [[ith i+1th] (partition 2 1 (:nodes road))]
;                      (case (:direction road)
;                        :--> {ith [i+1th]}
;                        :<-- {i+1th [ith]}
;                        :<-> (try {ith [i+1th], i+1th [ith]}
;                                  (catch IllegalArgumentException e
;                                    {ith [i+1th]})))))))))
;
;(defn write-csv
;  [csv-vec]
;  (with-open [writer (io/writer "adjacency-list.csv")]
;    (csv/write-csv writer
;                   (->> csv-vec
;                        (apply list)
;                        (map flatten)))))