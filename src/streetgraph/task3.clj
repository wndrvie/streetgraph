(ns streetgraph.task3
  (:require [streetgraph.pathfinding :refer [dijkstra unwind]]
            [streetgraph.osm.opening-backup :refer :all]
            [streetgraph.vis :refer :all]
            [taoensso.nippy :refer [thaw-from-file]]
            [clojure.pprint :refer [print-table]]
            [streetgraph.tsp :refer :all])
  (:import (java.io FileWriter)))

; adjacency matrix
(def adj-matx
  (into []
        (for [node dests
              :let [dists (dijkstra adj-list node)]]
          (mapv #(let [dist (-> dists % :dist)]
                   (if (= dist 0) Double/POSITIVE_INFINITY dist)) dests))))

; matrix of shortest paths
(def path-matx
  (into []
        (for [node dests
              :let [dists (dijkstra adj-list node)]]
          (mapv #(unwind dists %) dests))))

(defn draw-TS-tour
  [tour color filename]
  (render
    (:img-size map-props)
    (draw-on-city roads nodes
                  (into
                    (mapv #(draw-dots-seq :dots (-> path-matx (nth (first %)) (nth (second %)))
                                          :connected true
                                          :coords nodes
                                          :line-width 2
                                          :dot-diam 2
                                          :line-color color
                                          :dot-color color) (partition 2 1 (:nodes tour)))
                    (vector
                      (into
                        (draw-dots-seq :dots (take 10 (keys facils))
                                       :connected false
                                       :coords facils
                                       :dot-diam 5
                                       :dot-color "#EA4335")
                        (map #(vector :text {:font-family "Verdana" :font-size 25 :x (:x %2) :y (:y %2)} (str %1))
                             (range 0 (count adj-matx)) (map #(nth (vals facils) %) (:nodes tour)))))))
    (str "out/" filename)))

(defn task3
  []
  (let [nearest (closest-neighbor 0 adj-matx)
        insertn (insertion-heuristic adj-matx)]
    (do
      ; write tours info out
      (binding [*out* (new FileWriter "out/salesman_tours.txt")]
        (print-table [:algorithm :kms :nodes]
                     [(assoc nearest :algorithm "nearest neighbor"
                                     :nodes (mapv #(nth dests %) (:nodes nearest))
                                     :kms (* (:cost nearest) (:km map-props)))
                      (assoc insertn :algorithm "insertion heuristic"
                                     :nodes (mapv #(nth dests %) (:nodes insertn))
                                     :kms (* (:cost insertn) (:km map-props)))]))
      ; TS tour -> .svg
      (draw-TS-tour nearest "#c93e00" "tsp_nearest_neigh")
      (draw-TS-tour insertn "#00ffbf" "tsp_insertion_heur"))))
