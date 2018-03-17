(ns streetgraph.visualisation)
(require '[dali.io :as dali])

(defn render-svg
  "Takes roads' info and compiles it into dali doc,
  which can be rendered in .svg after"
  [roads coordinates]
  (dali/render-svg
    (into [:dali/page]
          (apply concat
                 (for [road roads]
                   (let [dots
                         (for [node (:nodes road)]
                           (apply vector
                                  (map (fn [axis]
                                         (-> (if (nil? (node coordinates))
                                               {:lat "0", :lon "0"}
                                               (node coordinates))
                                             (get axis)
                                             read-string
                                             (* 2000)
                                             (mod 2000)))
                                       '(:lon :lat))))]
                     (apply list
                            (apply
                              conj [:polyline {:stroke :grey :stroke-width 1 :fill :none}]
                              dots)
                            (map
                              (fn [dot]
                                [:circle {:fill :red} dot 1])
                              dots)))))) "visualisation.svg"))