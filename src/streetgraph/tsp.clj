(ns streetgraph.tsp
  (:require [streetgraph.pathfinding :refer [dijkstra unwind]]))

(defrecord Path [from to nodes cost])

(defrecord Tour [cost nodes])

;(doseq [line matx-of-paths]
;  (println line))

(defn extd-min
  "Extended min. Finds the minimum element in O(N) and returns its value and idx."
  [coll]
  (loop [min Double/POSITIVE_INFINITY
         idx 0
         curr-idx 0]
    (if (= (count coll) curr-idx)
      {:val min :idx idx}
      (let [ith (nth coll curr-idx)]
        (if (< ith min)
          (recur ith curr-idx (inc curr-idx))
          (recur min idx (inc curr-idx)))))))

(defn closest-neighbor
  "Given an adjacency list of a full graph, returns TS tour,
  made by closest neighbor algorithm.
  In: node to start with, adjacency matrix
  Out: "
  [start adj-matx]
  (let [N (count adj-matx)]
    (loop [seen #{start}
           tour [start]
           cost 0]
      (if (= (count tour) (count adj-matx))
        (->Tour cost (conj tour (first tour)))
        (let [curr (last tour)
              neigbs (-> adj-matx (nth curr))
              not-seen (doall (for [i (range 0 N)]
                                (if (not (contains? seen i))
                                  (nth neigbs i)
                                  Double/POSITIVE_INFINITY)))
              minm (extd-min not-seen)]
          (recur (conj seen (:idx minm))
                 (conj tour (:idx minm))
                 (+ cost (:val minm))))))))

(defn in?
  "True if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn insertion-heuristic
  [adj-matx]
  (let [N (count adj-matx)
        mins-in-ln (doall (for [line adj-matx] (extd-min line)))
        min-in-mtx (extd-min (for [min mins-in-ln] (:val min)))
        shrst-edge [(:idx min-in-mtx) (-> mins-in-ln (nth (:idx min-in-mtx)) :idx)]]
    (loop [tour #{shrst-edge}
           cost (:val min-in-mtx)
           unvi (into #{} (filterv #(not (in? shrst-edge %)) (range 0 N)))]
      (if (empty? unvi)
        (let [sorted-tour (into [] (sort-by first tour))
              tour (loop [tour (first sorted-tour)
                          curr (second tour)]
                     (if (= N (count tour))
                       tour
                       (let [next (nth sorted-tour curr)]
                         (recur (conj tour (second next)) (second next)))))]
          (->Tour cost (conj tour (first tour))))
        (let [widens (for [edge tour
                           v unvi
                           :let [ab (-> adj-matx (get (first edge)) (get (second edge)))
                                 av (-> adj-matx (get (first edge)) (get v))
                                 vb (-> adj-matx (get v) (get (second edge)))]]
                       {:instead-of edge
                        :new-vertex v
                        :add-dist   (-> av (+ vb) (- ab))})
              opt-idx (:idx (extd-min (mapv #(:add-dist %) widens)))
              opt (nth widens opt-idx)]
          (recur (-> tour
                     (disj (:instead-of opt))
                     (conj [(first (:instead-of opt)) (:new-vertex opt)]
                           [(:new-vertex opt) (second (:instead-of opt))]))
                 (+ cost (:add-dist opt)) (disj unvi (:new-vertex opt))))))))

