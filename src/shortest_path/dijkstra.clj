(ns shortest-path.dijkstra
  (:require [clojure.data.priority-map :refer [priority-map-keyfn-by]]))

;(defn sqr
;  [a]
;  (* a a))
;
;(defn euclid-distance
;  [a b]
;  (Math/sqrt
;    (apply + (for [ai a
;                   bi b]
;               (sqr (- ai bi))))))

(defn dijkstra
  "
  * g is a graph as adj-list: { :node-id { :adj-node-id dist-between, ... } }
  * src is a vertex to start: :node-id"
  [g src]
  (loop [priors (priority-map-keyfn-by :dist < src {:dist  0
                                                    :nodes [src]})
         curr (peek priors)
         unvi (apply hash-set (keys g))
         seen {}]
    (if (empty? priors)
      seen
      (let [curr-id (key curr)
            seen (assoc seen curr-id (val curr))
            adjs (map #(select-keys % unvi) (curr-id g))
            unvi (disj unvi curr-id)]
        (if (empty? adjs)
          (let [priors (dissoc priors curr-id)]
            (recur priors (peek priors) unvi seen))
          (let [cur-dist (get-in priors [curr-id :dist])
                cur-path (get-in priors [curr-id :nodes])
                adj-priors (for [adj (select-keys (curr-id g) unvi) ; adj is a map entry: node-id + dist-between
                                 :let [old-dist (get-in priors [(key adj) :dist]
                                                        Double/POSITIVE_INFINITY)
                                       new-dist (+ cur-dist (val adj))]
                                 :when (< new-dist old-dist)]
                             {(key adj) {:dist  new-dist
                                         :nodes (conj cur-path (key adj))}})
                priors (reduce merge (dissoc priors curr-id) adj-priors)]
            (recur priors (peek priors) unvi seen)))))))
