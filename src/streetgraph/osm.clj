(ns streetgraph.osm
  (:require [clojure.java.io :refer [resource file reader]]
            [clojure.xml :refer [parse]]
            [clojure.zip :refer [xml-zip]]
            [clojure.data.zip.xml :refer [xml-> attr attr=]]
            [clojure.data.xml :refer [parse]]
            [taoensso.nippy :refer [freeze-to-file]]))

; TODO lazy parse? OOM error when uberjar
(def osm-file (-> "arkhangelsk2.xml" resource file parse xml-zip))

(def ways (xml-> osm-file :way))

; { :min {:lat :lon } :max {:lat lon } }
(def bounds
  (let [bounds-tag (first (doall (xml-> osm-file :bounds)))]
    {:min {:lat (read-string (attr bounds-tag :minlat))
           :lon (read-string (attr bounds-tag :minlon))}
     :max {:lat (read-string (attr bounds-tag :maxlat))
           :lon (read-string (attr bounds-tag :maxlon))}}))

;; TESTED ;;
(defn transform-coords
  "coords { :lat :lon }
  bounds { :min { :lat :lon } :max { :lat :lon }
  img-size { :x :y } x y
  RETURNS: {:x .. y: .. }"
  [coords
   bounds
   img-size]
  (let [cart-map {:lat :y
                  :lon :x}
        gap (zipmap [:lat :lon]
                    (map #(- (get-in bounds [:max %])
                             (get-in bounds [:min %])) [:lat :lon]))]
    (zipmap (map #(cart-map %) (keys coords))
            (map #(let [axis (key %)]
                    (-> (- (coords axis)
                           (get-in bounds [:min axis]))
                        (* (img-size (cart-map axis)))
                        (/ (gap axis)))) coords))))

; REPL bench: 25 seconds
(defn get-coordinates
  "Takes zipped OSM-file and returns all nodes' geo information.
  Return fmt: { & :node-id {:lon node-lan, :lat node-lat }"
  [osm-file]
  (let [nodes (xml-> osm-file :node)]
    (into {}
          (for [n nodes]
            [(keyword (attr n :id))
             (transform-coords
               {:lat (read-string (attr n :lat)),
                :lon (read-string (attr n :lon))}
               bounds
               {:x 2000 :y 2000})]))))

(defn get-road-direction
  "Takes <tag>s of a <way> and returns way's direction.
  Return fmt: :--> | :<-- for roads with one way traffic, :<-> for others"
  [way-tags]
  (let [oneway-info-in (attr= :k "oneway")
        oneway-info
        (filter some?
                (for [tag way-tags]
                  (if (oneway-info-in tag)
                    (attr tag :v))))]
    (if (= oneway-info '("yes"))
      :-->
      (if (= oneway-info '("-1"))
        :<--
        :<->))))

; REPL bench: 20 seconds
(defn get-roads
  "Takes zipped <way> tags and returns all of those with 'highway' mark.
  Return fmt: ( & { :direction dir, :nodes ( & :node-id ) } ) "
  [ways]
  (do
    (println "Extracting roads...")
    (time
      (filter some?
              (let
                ; predicate, that returns true
                ; for <tag>s, containing "k"="highway" attribute
                [highway-info-in (attr= :k "highway")]
                ; checking ways
                (for [way ways]
                  (let [tags (xml-> way :tag)
                        nodes (xml-> way :nd)]
                    (if (some highway-info-in tags)
                      ; return a map
                      {:nodes
                       (->> nodes
                            (map #(attr % :ref))
                            (map keyword))
                       :direction
                       (get-road-direction tags)}))))))))

; TODO lazy-cat
; REPL bench: 134 msecs
(defn frequency-map
  "Takes roads and returns mapping between each node and adjacent roads.
  Return fmt: { & :node-id [ adjacent roads' indexes ] }"
  [roads]
  (frequencies
    (apply concat
           (for [road roads]
             (:nodes road)))))

; REPL bench: 448 msecs
(defn filter-roads
  "Removes unwanted nodes (anything except crossings and beginning + end of a road.
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

(defn filter-coordinates
  "Builds coordinates database.
  Only active nodes are saved."
  [coords
   roads]
  (persistent!
    (apply assoc! (transient {})
           (flatten (for [node (flatten (for [road roads] (:nodes road)))]
                      (list node (node coords)))))))

(defn euclid
  [a b]
  (Math/sqrt
    (apply +
           (map #(* % %)
                [(- (first a) (first b))
                 (- (second a) (second b))]))))

(defn adj-list
  [roads
   coords]
  (apply merge-with merge
         (lazy-cat
           (for [road roads
                 [ith i+1th] (partition 2 1 (:nodes road))
                 :let [dist-betw (euclid (vals (ith coords))
                                         (vals (i+1th coords)))]]
             (case (:direction road)
               :--> (merge {i+1th {}} {ith {i+1th dist-betw}})
               :<-- (merge {ith {}} {i+1th {ith dist-betw}})
               :<-> (merge {ith {i+1th dist-betw}}
                           {i+1th {ith dist-betw}}))))))

(def roads
  (-> ways
      get-roads
      filter-roads))

(def coords
  (-> osm-file
      get-coordinates
      (filter-coordinates roads)))

(defn freeze-meta
  [where-to]
  (do
    (freeze-to-file (str where-to "/roads.npy") roads)
    (freeze-to-file (str where-to "/adjacency_list.npy") (adj-list roads coords))
    (freeze-to-file (str where-to "/coordinates.npy") coords)))

(def lazy-osm-file
  (parse (reader "resources/arkhangelsk.xml")))


; lazy parse?
; { :nodes lazy list of maps
; (:id :lat :lon :facility) ,
; :ways lazy list of ways
; (:direct (nodes ids)) }

;(defn lazy-parse
;  "Returns a lazy sequence of nodes geo information.
;  Return fmt: { & :node-id {:lon node-lan, :lat node-lat }"
;  [osm-file]
;  (take-while #(not (= :way %))
;              (for [element (:content lazy-osm-file)]
;                (:tag element)))
;  )

;(def parsed
;  (for [tag (take 300 (:content lazy-osm-file))]
;    (case (:tag tag)
;      :node
;      (let [node-attrs (:attrs tag)]
;        {:id (:id node-attrs)
;         :lon (:lon node-attrs)
;         :lat (:lat node-attrs)})
;      nil)))
