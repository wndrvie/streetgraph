(ns streetgraph.pathfinding
  (:require [clojure.data.priority-map :refer [priority-map-keyfn-by]]
            [taoensso.tufte :refer [defnp p profiled profile]]
            [flatland.ordered.set :refer [ordered-set]]
            [linked.core :as linked]))

(defn unwind
  [dsts goal]
  (loop [cur goal
         path []]
    (if (nil? cur)
      path
      (recur (get-in dsts [cur :anc]) (conj path cur)))))

(defn dijkstra
  "
  * g is a graph as adj-list: { :node-id { :adj-node-id dist-between, ... } }
  * src is a vertex to start: :node-id"
  [g src]
  (loop [pque (priority-map-keyfn-by :dist < src {:dist 0 :anc  nil})
         curr (peek pque)
         seen {}]
    (if (empty? pque)
      seen
      (let [curr-id (key curr)
            _seen (conj seen curr)
            adjs (p :selecting-adjacent
                    (for [adj (curr-id g) :when (not (contains? _seen (key adj)))] adj))]
        (if (empty? adjs)
          (let [_pque (dissoc pque curr-id)]
            (recur _pque (peek _pque) _seen))
          (let [cur-dist (get-in pque [curr-id :dist])
                adj-priors (for [adj adjs ; adj is a map entry: node-id + dist-between
                                 :let [old-dist (get-in pque [(key adj) :dist]
                                                        Double/POSITIVE_INFINITY)
                                       new-dist (+ cur-dist (val adj))]
                                 :when (< new-dist old-dist)]
                             {(key adj) {:dist new-dist
                                         :anc  curr-id}})
                _pque (p :forming-new-pque (into (dissoc pque curr-id) adj-priors))]
            (recur _pque (peek _pque) _seen)))))))

(defn levit
  [start graph]
  (loop [d {start {:dist 0 :anc nil}}
         M0 #{}
         M1 (linked/set start)
         !M1 (linked/set)
         M2 (p :init-M2 (reduce conj #{} (keys graph)))]
    (if (and (empty? M1)
             (empty? !M1))
      d
      (let [cur (if (empty? !M1) (p :getting-first (first M1)) (p :getting-first-from! (first !M1)))
            adjs (get graph cur)
            adjs-from (p :sorting-adjacences
                         (reduce-kv (fn [m k v]
                                      (if (contains? M2 k)
                                        (merge-with conj m {:M2 k})
                                        (if (or (contains? M1 k)
                                                (contains? M2 k))
                                          (merge-with conj m {:M1 k})
                                          (merge-with conj m {:M0 k}))))
                                    {:M0 [] :M1 [] :M2 []} adjs))

            _M1 (p :building-M1 (reduce conj (disj M1 cur) (get adjs-from :M2)))
            _M2 (p :building-M2 (reduce disj M2 (get adjs-from :M2)))
            _M0 (p :building-M0
                   (reduce (fn [set key]
                             (if (< (+ (get-in d [cur :dist]) (get adjs key))
                                    (get-in d [key :dist] Double/POSITIVE_INFINITY))
                               (disj set key)
                               set)) (conj M0 cur) (get adjs-from :M0)))
            _!M1 (p :building-M1!
                    (reduce (fn [set key]
                                            (if (< (+ (get-in d [cur :dist]) (get adjs key))
                                                   (get-in d [key :dist] Double/POSITIVE_INFINITY))
                                              (conj set key)
                                              set)) (disj !M1 cur) (get adjs-from :M0)))
            _d (p :building-d
                  (reduce (fn [d* adj]
                         (if (< (+ (get-in d* [cur :dist]) (val adj))
                                (get-in d* [(key adj) :dist] Double/POSITIVE_INFINITY))
                           (assoc d* (key adj) {:dist (+ (get-in d* [cur :dist]) (val adj))
                                                :anc cur})
                           d*)) d adjs))]
        (recur _d _M0 _M1 _!M1 _M2)))))

(defn A*
  [start goal heur graph]
  (let [f #(+ (:h %) (:g %))]
    (loop [now (priority-map-keyfn-by f <
                                      start {:h (heur start goal) :g 0 :anc nil})
           seen {}]
      (if (empty? now)
        ; no ways
        nil
        (if (= (key (peek now))
               goal)
          {:dist (:g (val (peek now))) :path (unwind (conj seen (peek now)) goal)}
          (let [cur (peek now)
                adjs (filter #(not (contains? seen (key %))) (get graph (key cur)))
                -seen (conj seen cur)
                -now (into
                             (dissoc now (key cur))
                             (for [adj adjs
                                   :let [score {:g (+ (val adj) (get (val cur) :g))
                                                :h (heur (key adj) goal)}]
                                   :when (or (not (or (contains? seen (key adj))
                                                      (contains? now (key adj))))
                                             (< (get score :g)
                                                (get-in now [(key adj) :g] Double/POSITIVE_INFINITY)))]
                               [(key adj) (assoc score
                                            :anc (key cur))]))]
            (recur -now -seen)))))))

(defn init-heur-with
  ";-;"
  [heur
   coords]
  (fn [id1 id2]
    (heur (vals (get coords id1))
          (vals (get coords id2)))))

(defn euclid
  [a b]
  (Math/sqrt
    (apply +
           (map #(* % %)
                [(- (first a) (first b))
                 (- (second a) (second b))]))))

(defn manh
  [a b]
  (/ (+ (Math/abs ^double (- (first a) (first b)))
        (Math/abs ^double (- (second a) (second b))))
     2))

(defn cheb
  [a b]
  (max (Math/abs ^double (- (first a) (first b)))
       (Math/abs ^double (- (second a) (second b)))))