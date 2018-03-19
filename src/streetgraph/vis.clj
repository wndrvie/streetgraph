(ns streetgraph.vis)
;; VISUALISATION

(require '[dali.io :as dali])

(defn render-svg
  "Takes roads' info and compiles it into dali doc,
  which is rendered .svg after"
  [roads coordinates]
  (dali/render-svg
    (into [:dali/page]
          (apply concat
                 (for [road roads]
                   (let [dots
                         (for [node (:nodes road)]
                           (apply vector
                                  (map (fn [inp]
                                         (-> (if (nil? (node coordinates))
                                               {:lat "0", :lon "0"}
                                               (node coordinates))
                                             (get (:axis inp))
                                             read-string
                                             (- (:edge inp))
                                             (/ (:diff inp))
                                             (* 2000)))
                                       '({:axis :lon, :edge 40.1028, :diff 0.7512}
                                          {:axis :lat, :edge 64.4497, :diff 0.2793}))))]
                     (apply list
                            (apply
                              conj [:polyline {:stroke :grey :stroke-width 1 :fill :none}]
                              dots)
                            (map
                              (fn [dot]
                                [:circle {:fill :red} dot 1])
                              dots)))))) "visualisation.svg"))