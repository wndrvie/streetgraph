(ns streetgraph.core
  (:require [streetgraph.vis :refer [visualise]])
  (:gen-class))

;(require
;  '[streetgraph.csv :as csv]
;  '[streetgraph.osm :as osm]
;  '[streetgraph.vis :as viz]
;  '[shortest-path.dijkstra :refer :all]
;  '[taoensso.nippy :as nippy])

(defn -main
  "What else to say?"
  [& args]
  (time
    ;(osm/freeze-meta "resources")
    (visualise)
    ;(let [roads (nippy/thaw-from-file "resources/roads.npy")
    ;      coordinates (nippy/thaw-from-file "resources/coordinates.npy")
    ;      adjacency-list (nippy/thaw-from-file "resources/adjacency_list.npy")
    ;      ;shortest (-> (dijkstra adjacency-list :3021637605)
    ;      ;             (get :1120392327)
    ;      ;             list)
    ;      shortest {:dist 0.0018408760826655534,
    ;              :nodes [:3021637605 :3021638539 :4294338262 :1120392218 :1120392327]}                           ;
    ;      ;prep-roads (viz/prepare roads coordinates :grey 1 :red 1)
    ;      ;prep-shpaths (viz/prepare shortest coordinates :blue 2 :green 3)
    ;      prep-shpaths (viz/roads-to-svg shortest coordinates)
    ;      ]
    ;  (do
    ;    (viz/render prep-shpaths)
    ;    (time (get adjacency-list :3021638539))))
    ))

    ;
    ;(let [adjacency-list (time (nippy/thaw-from-file "resources/adjacency_list.npy"))]
    ;  (println (-> (time (dijkstra adjacency-list :3021637605))
    ;               (get :1120392327))))

;(let [roads-db "resources/roads.npy"]
;  (do
;    (osm/set-roads-db "resources/roads.npy")
;    (osm/set-coordinates-db "resources/coordinates.npy")))

;(-> roads
;    csv/build-adj-list
;    csv/write-csv)