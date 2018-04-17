(ns streetgraph.vis
  (:require [dali.io :refer [render-svg render-png]]
            [taoensso.nippy :refer [thaw-from-file]]))
;; VISUALISATION

(def roads (thaw-from-file "resources/roads.npy"))
(def coords (thaw-from-file "resources/coordinates.npy"))
;(def adj-list (thaw-from-file "resources/adjacency_list.npy"))

;(defn prepare
;  [roads coordinates road-color stroke-width node-color dot-diam]
;  (lazy-cat
;    (for [road roads]
;      (let [dots
;            (for [node (:nodes road)]
;              (apply vector
;                     (map (fn [inp]
;                            ; TODO fix
;                            (-> (get coordinates node {:lat "0", :lon "0"})
;                                (get (:axis inp))
;                                (- (:edge inp))
;                                (/ (:diff inp))
;                                (* 1500)))
;                          '({:axis :lon, :edge 40.1028, :diff 0.7512}
;                             {:axis :lat, :edge 64.4497, :diff 0.2793}))))]
;        (apply list
;               (persistent!
;                 (reduce conj!
;                         (transient [:polyline {:stroke road-color :stroke-width stroke-width :fill :none}]) dots))
;               (map (fn [dot]
;                      [:circle {:fill node-color} dot dot-diam]) dots)))))
;  )

;(defn coords-to-vec
;  [coords-as-map]
;  (mapv #(-> coords-as-map
;             (get (:axis %))
;             (- (:edge %))
;             (/ (:diff %))
;             (* 1500))
;        [{:axis :lon, :edge 40.1028, :diff 0.7512}
;         {:axis :lat, :edge 64.4497, :diff 0.2793}]))

;(defn roads-to-svg
;  [road-seq coords ]
;  ;road-props node-props
;  (apply concat
;         (for [roads road-seq
;               :let [nodes (:nodes roads)
;                     dots (map coords-to-vec (map #(get coords %) nodes))]]
;           (apply list
;                  (persistent!
;                    (reduce conj!
;                            (transient [:polyline {:stroke :grey :stroke-width 1 :fill :none}]) dots))
;                  (map #(vector :circle {:fill :red} % 1) dots)))))

(defn roads-to-svg
  [road-seq coords]
  ;road-props node-props
  (apply concat
         (for [roads road-seq
               :let [nodes (:nodes roads)
                     dots (map #(vector (:y (get coords %)) (:x (get coords %))) nodes)]]
           (apply list
                  (persistent!
                    (reduce conj!
                            (transient [:polyline {:stroke :grey :stroke-width 1 :fill :none}]) dots))
                  (map #(vector :circle {:fill :red} % 1) dots)))))

(defn render
  [prepared-roads]
  (render-svg
    (persistent!
      (reduce conj! (transient [:dali/page]) prepared-roads))
    "vis.svg"))

(defn visualise
  []
  (render (roads-to-svg roads coords)))

;(defn render-svg
;  [roads coordinates]
;  (do
;    (println "Rendering svg...")
;    (time
;      (render-svg
;        (persistent!
;          (reduce conj! (transient [:dali/page])
;                  (lazy-cat
;                    (for [road roads]
;                      (let [dots
;                            (for [node (:nodes road)]
;                              (apply vector
;                                     (map (fn [inp]
;                                            ; TODO fix
;                                            (-> (get coordinates node {:lat "0", :lon "0"})
;                                                (get (:axis inp))
;                                                read-string
;                                                (- (:edge inp))
;                                                (/ (:diff inp))
;                                                (* 2000)))
;                                          '({:axis :lon, :edge 40.1028, :diff 0.7512}
;                                             {:axis :lat, :edge 64.4497, :diff 0.2793}))))]
;                        (apply list
;                               (persistent!
;                                 (reduce conj!
;                                         (transient [:polyline {:stroke :grey :stroke-width 1 :fill :none}]) dots))
;                               (map (fn [dot]
;                                      [:circle {:fill :red} dot 1]) dots)))))
;                  ))
;        "visualisation.svg"))))

;(into [:dali/page]
;      (apply concat
;             (for [road roads]
;               (let [dots
;                     (for [node (:nodes road)]
;                       (apply vector
;                              (map (fn [inp]
;                                     ; TODO fix
;                                     (-> (get coordinates node {:lat "0", :lon "0"})
;                                         (get (:axis inp))
;                                         read-string
;                                         (- (:edge inp))
;                                         (/ (:diff inp))
;                                         (* 1500)))
;                                   '({:axis :lon, :edge 40.1028, :diff 0.7512}
;                                      {:axis :lat, :edge 64.4497, :diff 0.2793}))))]
;                 (apply list
;                        (apply
;                          conj [:polyline {:stroke :grey :stroke-width 1 :fill :none}]
;                          dots)
;                        (map
;                          (fn [dot]
;                            [:circle {:fill :red} dot 1])
;                          dots))))))