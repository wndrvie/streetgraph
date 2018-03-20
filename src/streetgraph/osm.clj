(ns streetgraph.osm)
;; OSM PARSING & PROCESSING

(require '[clojure.java.io :as io]
         '[clojure.xml :as xml]
         '[clojure.zip :as zip]
         '[clojure.data.zip.xml :as zip-xml])

(def osm-file (-> "arkhangelsk.xml" io/resource io/file xml/parse zip/xml-zip))
(def ways (zip-xml/xml-> osm-file :way))

; for returns lazy
(defn get-coordinates
  "Takes zipped OSM-file and returns all nodes' geo information.
  Return fmt: { & :node-id {:lon node-lan, :lat node-lat }"
  [osm-file]
  (let [nodes (zip-xml/xml-> osm-file :node)]
    (into {}
          (for [n nodes]
            [(keyword (zip-xml/attr n :id))
             {:lat (zip-xml/attr n :lat),
              :lon (zip-xml/attr n :lon)}]))))

(defn get-road-direction
  "Takes <tag>s of a <way> and returns way's direction.
  Return fmt: :--> | :<-- for roads with one way traffic, :<-> for others"
  [way-tags]
  (let [oneway-info-in (zip-xml/attr= :k "oneway")
        oneway-info
        (filter some?
                (for [tag way-tags]
                  (if (oneway-info-in tag)
                    (zip-xml/attr tag :v))))]
    (if (= oneway-info '("yes"))
      :-->
      (if (= oneway-info '("-1"))
        :<--
        :<->))))

(defn get-roads
  "Takes zipped <way> tags and returns all of those with 'highway' mark.
  Return fmt: ( & { :direction dir, :nodes ( & :node-id ) } ) "
  [ways]
  (filter some?
          (let
            ; predicate, that returns true
            ; for <tag>s, containing "k"="highway" attribute
            [highway-info-in (zip-xml/attr= :k "highway")]
            ; checking ways
            (for [way ways]
              (let [tags (zip-xml/xml-> way :tag)
                    nodes (zip-xml/xml-> way :nd)]
                (if (some highway-info-in tags)
                  ; return a map
                  {:nodes
                   (->> nodes
                        (map #(zip-xml/attr % :ref))
                        (map keyword))
                   :direction
                   (get-road-direction tags)}))))))

(defn frequency-map
  "Takes roads and returns mapping between each node and adjacent roads.
  Return fmt: { & :node-id [ adjacent roads' indexes ] }"
  [roads]
  (apply merge-with into
         (apply concat
                (for [i (range (count roads))]
                  (for [node (:nodes (nth roads i))]
                    {node [i]})))))

(defn filter-roads
  [roads]
  (let [frequency-map (frequency-map roads)]
    (for [road roads]
      (let [nodes (:nodes road)
            first (first nodes)
            last (last nodes)]
        (assoc road
          :nodes (conj (filterv some?
                                (into [first]
                                      (for [node nodes]
                                        (if (and (> (count (get frequency-map node)) 1)
                                                 (not (= last node))
                                                 (not (= first node)))
                                          node)))) last))))))