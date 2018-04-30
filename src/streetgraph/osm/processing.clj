(ns streetgraph.osm.processing)
;; CONTAINS FUNCS FOR PROCESSING PARSED OSM DATA ;;

(defn set-mapping
  "Returns a function, that will map {:lat :lon} coordinates
  to {:x :y}. The mapping will be based on lat-lon bounds and desired size of result image. "
  [bounds img-size]
  (fn
    [lon-lat]
    (let [cart-map {:lat :y
                    :lon :x}
          gap (zipmap [:lat :lon]
                      (map #(- (get-in bounds [:max %])
                               (get-in bounds [:min %])) [:lat :lon]))]
      (zipmap (map #(cart-map %) (keys lon-lat))
              (map #(let [axis (key %)
                          val (-> (- (lon-lat axis)
                                     (get-in bounds [:min axis]))
                                  (* (img-size (cart-map axis)))
                                  (/ (gap axis)))]
                      (if (= axis :lat) (- (img-size :y) val) val)) lon-lat)))))

(defn frequency-map
  "Takes roads and returns mapping between each node and adjacent roads.
  Return fmt: { & :node-id [ adjacent roads' indexes ] }"
  [roads]
  (frequencies
    (reduce into []
            (for [road roads]
              (:nodes road)))))

(defn euclid
  [a b]
  (Math/sqrt
    (apply +
           (map #(* % %)
                [(- (first a) (first b))
                 (- (second a) (second b))]))))

(defn get-closest-node
  "Iterating over all base coordinates (filtered).
  Finds the node, closest to the given one, and returns its id.
  Arg: {:x :y} of the current node.
  Out: :id of the closest (euclid) node."
  [node-coords
   coords-base]
  (let [given-node-coords node-coords]
    (loop [ans (key (first coords-base))
           dist (euclid (vals given-node-coords) (vals (val (first coords-base))))
           nodes-seq (next coords-base)]
      (if (nil? nodes-seq)
        {:closest ans
         :dist dist}
        ; ans
        (let [cur (first nodes-seq)
              cur-node-coords (val cur)
              cur-dist (euclid (vals cur-node-coords) (vals given-node-coords))]
          (if (< cur-dist dist)
            (recur (key cur) cur-dist (next nodes-seq))
            (recur ans dist (next nodes-seq))))))))

(defn filter-roads
  "Removes unwanted nodes (anything except crossings and beginning + end of a road).
  Returns: { :direction :--> / :<-- / :<->  :nodes [ :node-id ... ] ... }"
  [roads]
  (let [frequency-map (frequency-map roads)]
    (for [road roads]
      (let [nodes (:nodes road)
            first (first nodes)
            last (last nodes)]
        (assoc road
          :nodes (conj (filter some?
                               (into [first]
                                     (for [node nodes]
                                       (if (and (> (get frequency-map node) 1)
                                                (not (= last node))
                                                (not (= first node)))
                                         node)))) last))))))

(defn filter-coords
  "Builds coordinates database.
  Only active nodes are saved."
  [coords-base
   roads-base]
  (reduce merge
          (reduce into
                  (for [road roads-base
                        :let [nodes (:nodes road)]]
                    (map #(array-map % (get coords-base %)) nodes)))))

(defn adj-list
  [roads-base
   coords-base]
  (apply merge-with merge
         (lazy-cat
           (for [road roads-base
                 [ith i+1th] (partition 2 1 (:nodes road))
                 :let [dist-betw (euclid (vals (ith coords-base))
                                         (vals (i+1th coords-base)))]]
             (case (:direction road)
               :--> (merge {i+1th {}} {ith {i+1th dist-betw}})
               :<-- (merge {ith {}} {i+1th {ith dist-betw}})
               :<-> (merge {ith {i+1th dist-betw}}
                           {i+1th {ith dist-betw}}))))))