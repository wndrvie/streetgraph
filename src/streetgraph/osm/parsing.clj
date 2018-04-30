(ns streetgraph.osm.parsing
  ;; CONTAINS FUNCTIONS FOR PARSING OSM-XML FILE AND EXTRACTIN
  ;; INFORMATION OUTTA THERE
  (:require [clojure.data.zip.xml :refer [xml-> attr attr=]]))

(defn get-coords
  "Takes zipped OSM-file and function lon-lat -> x y
  and returns all nodes' geo information.
  out: { :id {:x :y } ... }"
  [osm-file
   lon|lat->x|y]
  (let [nodes (xml-> osm-file :node)]
    (into {}
          (for [n nodes]
            [(keyword (attr n :id))
             (lon|lat->x|y
               {:lat (read-string (attr n :lat)),
                :lon (read-string (attr n :lon))})]))))

(defn get-road-dir
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
  (filter some?
          (let
            [highway-info-in (attr= :k "highway")]
            (for [way ways]
              (let [tags (xml-> way :tag)
                    nodes (xml-> way :nd)]
                (if (some highway-info-in tags)
                  {:nodes
                   (->> nodes
                        (map #(attr % :ref))
                        (map keyword))
                   :direction
                   (get-road-dir tags)}))))))

(defn get-facilities
  "out: { :id {:x :y} ... }"
  [osm-file lonlat->xy facility]
  (into {}
        (for [node (xml-> osm-file :node)
              :let [tags (xml-> node :tag)
                    title-tag (first (filter (attr= :k "name") tags))
                    title (if (nil? title-tag) "no title" (attr title-tag :v))]
              :when (some (attr= :v (str facility)) tags)]
          [(keyword (attr node :id))
           (assoc
             (lonlat->xy {:lat (read-string (attr node :lat))
                          :lon (read-string (attr node :lon))})
             :title title)])))