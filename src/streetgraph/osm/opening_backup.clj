(ns streetgraph.osm.opening-backup
  (:require [taoensso.nippy :refer [thaw-from-file]]))

(def path "resources")

(def roads
  (thaw-from-file (str path "/roads.npy")))

(def nodes
  (thaw-from-file (str path "/nodes.npy")))

(def adj-list
  (thaw-from-file (str path "/adj_list.npy")))

(def facils
  (thaw-from-file (str path "/facils.npy")))

(def dests
  (mapv #(:closest (val %)) (take 10 facils)))

(def map-props
  (thaw-from-file (str path "/mapping_props.npy")))