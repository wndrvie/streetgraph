(ns streetgraph.osm-processing)

(require '[clojure.java.io :as io]
         '[clojure.xml :as xml]
         '[clojure.zip :as zip]
         '[clojure.data.zip.xml :as zip-xml])

(def osm-file (-> "arkhangelsk.xml" io/resource io/file xml/parse zip/xml-zip))
(def ways (zip-xml/xml-> osm-file :way))
(def nodes (zip-xml/xml-> osm-file :node))

(defn get-coordinates
  [osm-file]
  (let [nodes (zip-xml/xml-> osm-file :node)]
    (into {}
          (for [n nodes]
            [(keyword (zip-xml/attr n :id))
             {:lat (zip-xml/attr n :lat),
              :lon (zip-xml/attr n :lon)}]))))

(defn get-road-direction
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
  "Parses all <way> tags;
  gets all of those with 'highway' mark"
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
                  (let [road-entry (transient {})]
                    (do
                      (assoc! road-entry
                              :nodes
                              ; take out nodes' IDs
                              (->> nodes
                                   (map #(zip-xml/attr % :ref))
                                   (map keyword))
                              :direction
                              (get-road-direction tags))
                      (persistent! road-entry)))))))))

(defn create-frequency-map
  [roads]
  (let [freq-map (transient {})]
    (do
      (doseq [i (range (count roads))]
        (let [ith-road (nth roads i)]
          (doseq [node (:nodes ith-road)]
            ; get corresponding freqmap entry
            (let [fmap-entry (node freq-map)]
              (assoc! freq-map node
                      (if (nil? fmap-entry)
                        [i]
                        (conj fmap-entry i)))))))
      (persistent! freq-map))))

(defn filter-roads
  "Takes roads and removes all nodes,
  excepting first, last ones and crossings"
  [roads]
  (let [frequency-map
        (create-frequency-map roads)
        filtered-roads
        (transient
          ; initialisation: we set
          (apply vector
                 ; of
                 (for [r roads]
                   ; where every entry
                   ; keeps info about
                   {:direction (:direction r)
                    :nodes     (vector
                                 (first (:nodes r))
                                 (last (:nodes r)))})))]
    (do
      ; for every in
      (doseq [node-entry frequency-map]
        (let [road-list (val node-entry)
              node-id (key node-entry)]
          ; = if a node is present in more than 1 roads (crossing)
          (if (> (count road-list) 1)
            ; process every road of these
            (doseq [road-id road-list]
              (let [road-entry (nth filtered-roads road-id)]
                ; and push the node in each of them
                (if (not (some #{node-id} (list (first (:nodes road-entry))
                                                (last (:nodes road-entry)))))
                  ; TODO find a better way to put parts of filtered road together
                  (assoc! filtered-roads road-id
                          (assoc road-entry :nodes
                                            (apply vector (pop (:nodes road-entry))
                                                   (list node-id (last (:nodes road-entry)))))))))))))
    (persistent! filtered-roads)))