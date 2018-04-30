(ns streetgraph.task2
  (:require [streetgraph.pathfinding :refer :all]
            [streetgraph.osm.processing :refer [set-mapping get-closest-node]]
            [streetgraph.vis :refer [roads-to-svg render draw-dots-seq draw-city draw-shortest-paths]]
            [taoensso.nippy :refer [thaw-from-file]]
            [taoensso.tufte :as tufte :refer [p profile]]
            [clojure.data.csv :refer [write-csv]]
            [clojure.pprint :refer [print-table]]
            [clojure.java.io :as io])
  (:import (java.io FileWriter)))

(tufte/add-basic-println-handler! {})
(tufte/set-ns-pattern! "streetgraph.task2")

(def path "resources")

(def roads
  (thaw-from-file (str path "/roads.npy")))

(def nodes
  (thaw-from-file (str path "/nodes.npy")))

(def adj-list
  (thaw-from-file (str path "/adj_list.npy")))

(def facils
  (thaw-from-file (str path "/facils.npy")))

(def map-props
  (thaw-from-file (str path "/mapping_props.npy")))

(def spher->cart
  (set-mapping (:bounds map-props) (:img-size map-props)))

(defn run-task-2
  [lon lat]
  (let [cur-loc (spher->cart {:lon lon :lat lat})
        src (get-closest-node cur-loc nodes)
        picked-facils (take 10 facils)
        get-paths-from (fn [dsts goals]
                         (sort-by :dist (map #(hash-map :facil (key %)
                                                        :dist (get-in dsts [(:closest (val %)) :dist])
                                                        :path (unwind dsts (:closest (val %)))) goals)))
        dijk (dijkstra adj-list (:closest src))
        d-paths (get-paths-from dijk picked-facils)

        levit (levit (:closest src) adj-list)
        l-paths (get-paths-from levit picked-facils)

        a*-paths (into [] (sort-by :dist
                                   (map #(A* (:closest src) (:closest (val %))
                                             (init-heur-with manh nodes) adj-list) picked-facils)))
        timings (for [p d-paths
                      :let [facil-id (:facil p)]]
                  {:title   (-> facils facil-id :title)
                   :minutes (-> (+ (:dist src) (:dist p))
                                (* (-> map-props :km))
                                (/ 40.) (* 60) (.intValue))})

        prepare-visual (fn [paths]
                         (reduce conj (into
                                        (into (draw-city roads nodes)
                                              (draw-shortest-paths paths nodes))
                                        (draw-dots-seq :dots (keys picked-facils)
                                                       :connected false
                                                       :coords facils
                                                       :dot-diam 4
                                                       :dot-color "#EA4335"))
                                 [[:circle {:fill "#ffcc5c"} [(:x cur-loc) (:y cur-loc)] 7]
                                  [:text {:font-family "Verdana" :font-size 14 :x (:x cur-loc) :y (:y cur-loc)} "вы здесь"]]))
        ]
    (do
      (binding [*out* (new FileWriter "out/time_of_arrival.txt")]
        (print-table [:title :minutes] (sort-by :minutes timings)))
      (with-open [writer (io/writer "out/shortest_paths.csv")]
        (write-csv writer
                   (into [] (map #(into [] (cons (:facil %) (:path %))) d-paths))))
      (render (:img-size map-props)
              (prepare-visual d-paths)
              "out/dijkstra")
      (render (:img-size map-props)
              (prepare-visual l-paths)
              "out/levit")
      (render (:img-size map-props)
              (prepare-visual a*-paths)
              "out/astar"))))

(defn test-performance
  "Tests Levit's, Dijkstra's & A* algorithms' performance.
  Writes statistics to out/performance_testing.txt"
  []
  (binding [*out* (new FileWriter "out/performance_testing.txt")]
    (let [random-srcs (repeatedly 100 #(:closest (get-closest-node {:x (rand-int (:x (:img-size map-props)))
                                                                    :y (rand-int (:y (:img-size map-props)))} nodes)))
          goals (into [] (take 10 (map #(:closest (val %)) facils)))
          manh-heur (init-heur-with manh nodes)
          cheb-heur (init-heur-with cheb nodes)
          eucl-heur (init-heur-with euclid nodes)
          stats (second (tufte/profiled {}
                                        (dorun (for [src random-srcs]
                                                 (p :dijkstra (dorun (map #(unwind (dijkstra adj-list src) %) goals)))))
                                        (dorun (for [src random-srcs]
                                                 (p :levit (dorun (map #(unwind (levit src adj-list) %) goals)))))
                                        (dorun (for [src random-srcs
                                                     goal goals]
                                                 (p :A*+manhattan (A* src goal manh-heur adj-list))))
                                        (dorun (for [src random-srcs
                                                     goal goals]
                                                 (p :A*+chebyshev (A* src goal cheb-heur adj-list))))
                                        (dorun (for [src random-srcs
                                                     goal goals]
                                                 (p :A*+euclid (A* src goal eucl-heur adj-list))))
                                        ))
          formatted (tufte/format-pstats @stats)]
      (println (.replace formatted "\n" "\r\n")))))