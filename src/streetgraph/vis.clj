(ns streetgraph.vis
  (:require [dali.io :refer [render-svg render-png]]
            [taoensso.nippy :refer [thaw-from-file]]))
;; VISUALISATION

(defn roads-to-svg
  ([road-seq coords]
    (roads-to-svg road-seq coords {:stroke "#c0d6e4" :stroke-width 1 :fill :none} {:fill "#6497b1"}))
  ([road-seq coords road-props node-props]
   (reduce into []
           (for [roads road-seq
                 :let [nodes (:nodes roads)
                       dots (pmap #(vector (:x (get coords %)) (:y (get coords %))) nodes)]]
             (into
               (vector (reduce conj [:polyline road-props] dots))
               (map #(vector :circle node-props % 1) dots))))))

(defn draw-dots-seq
  "Returns dali-formatted roads."
  [& {:keys [dots coords connected line-color line-width dot-color dot-diam]
      :or {connected false
           line-color "#c0d6e4"
           line-width 1
           dot-color "#6497b1"
           dot-diam 1}}]
  (let [dots'-coords-seq (mapv #(vector (:x (get coords %)) (:y (get coords %))) dots)
        line (if connected
               (vector (reduce conj [:polyline {:stroke line-color :stroke-width line-width :fill :none}] dots'-coords-seq))
               [])]
    (into line
      (map #(vector :circle {:fill dot-color} % dot-diam) dots'-coords-seq))))

(defn draw-city
  "Returns dali representation of the city roads.
  Needs to be rendered after."
  [roads nodes]
  (reduce into []
          (for [road roads]
            (draw-dots-seq
              :dots (:nodes road)
              :coords nodes
              :connected true
              :line-color "#c0d6e4"
              :line-width 1
              :dot-color "#6497b1"
              :dot-diam 1))))

(defn render
  "Renders dali-formatted data into svg.
  In:
  * prep-data - dali formatted data;
  * img-size - {:x :y};
  * filename - guess what.
  Out:
  an svg file out there"
  [img-size prep-data filename]
  (render-svg
    (reduce conj [:dali/page {:width (get img-size :x)
                              :height (get img-size :y)}] prep-data)
    (str filename ".svg")))

(defn draw-shortest-paths
  "A specific func for drawing algorithms' results.
  In:
  * a sorted map { :dist, :path [ ... ] }. The entries need to be sorted
  by distance. The shortest path will be drawn with another color.
  * the geo coordinates base.
  Out:
  dali-formatted roads."
  [paths coords]
  (let [not-opt-ways (mapv #(draw-dots-seq :dots (:path %)
                                           :connected true
                                           :coords coords
                                           :line-color "#a96e5b"
                                           :dot-diam 1
                                           :dot-color "#c1502e") (rest paths))]
    (reduce (fn [old-coll new-coll]
              (reduce conj old-coll new-coll)) []
            (conj not-opt-ways (draw-dots-seq :dots (:path (first paths))
                                              :connected true
                                              :coords coords
                                              :line-color "#34A853"
                                              :line-width 2
                                              :dot-diam 2
                                              :dot-color "#34A853")))))

(defn draw-on-city
  ""
  [roads nodes list-of-roads]
  (reduce (fn [old-coll new-coll]
            (reduce conj old-coll new-coll)) (draw-city roads nodes) list-of-roads))