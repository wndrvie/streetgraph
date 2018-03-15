(ns streetgraph.core
  (:gen-class))
(require '[clojure.java.io :as io]
         '[clojure.xml :as xml]
         '[clojure.zip :as zip]
         '[clojure.data.zip.xml :as zip-xml]
         '[dali.io :as dali]
         '[clojure.data.csv :as csv])

(def osm-file (-> "arkhangelsk.xml" io/resource io/file xml/parse zip/xml-zip))
(def ways (zip-xml/xml-> osm-file :way))

(defn get-road-direction
  [way-tags]
  (let [oneway-info-in (zip-xml/attr= :k "oneway")]
    (let [oneway-info
          (filter some?
                  (for [tag way-tags]
                    (if (oneway-info-in tag)
                      (zip-xml/attr tag :v))))]
      (if (= oneway-info '("yes"))
        :-->
        (if (= oneway-info '("-1"))
          :<--
          :<->)))))

(defn get-roads
  "Parses all <way> tags;
  gets all of those with 'highway' mark"
  [ways]
  (filter some?
          (for [way ways]
            (let [tags (zip-xml/xml-> way :tag)
                  nodes (zip-xml/xml-> way :nd)
                  highway-info-in (zip-xml/attr= :k "highway")]
              (if (some highway-info-in tags)
                (let [road-entry (atom {})]
                  (do
                    (swap! road-entry assoc :nodes
                           (for [n nodes]
                             (keyword (zip-xml/attr n :ref))))
                    (swap! road-entry assoc :direction
                           (get-road-direction tags))
                    @road-entry)))))))

(defn filter-roads
  "Takes roads and removes all nodes,
  excepting first, last ones and crossings"
  [roads]
  (let [frequency-map (atom {})
        filtered-roads (atom
                         (apply vector
                                (for [r roads]
                                  {:direction (:direction r)
                                   :nodes     (vector
                                                (first (:nodes r))
                                                (last (:nodes r)))})))]
    (do
      (doseq [i (range (count roads))]
        (let [road (nth roads i)]
          (doseq [node (:nodes road)]
            (let [gotcha (get @frequency-map node)]
              (if (nil? gotcha)
                (swap! frequency-map assoc node [i])
                (swap! frequency-map assoc node (conj gotcha i)))))))
      (doseq [node-entry @frequency-map]
        (let [road-list (val node-entry)
              node-id (key node-entry)]
          (if (> (count road-list) 1)
            (doseq [road-id road-list]
              (let [road-entry (nth @filtered-roads road-id)]
                (if (not (some #{node-id} (take 2 (:nodes road-entry))))
                  (swap! filtered-roads assoc road-id
                         (assoc road-entry :nodes
                                           (conj (:nodes road-entry) (key node-entry)))))))))))
    @filtered-roads))

(defn prepare-image
  "Takes roads' info and compiles it into dali doc,
  which can be rendered in .svg after"
  [roads]
  (let [coordinates (atom {})]
    (do
      (doseq [n (zip-xml/xml-> osm-file :node)]
        (swap! coordinates assoc (keyword (zip-xml/attr n :id))
               {:lat (zip-xml/attr n :lat),
                :lon (zip-xml/attr n :lon)}))
      (into [:dali/page]
            (apply concat
                   (for [road roads]
                     (let [dots
                           (for [node (:nodes road)]
                             (apply vector
                                    (map (fn [axis]
                                           (-> (if (nil? (node @coordinates))
                                                 {:lat "0", :lon "0"}
                                                 (node @coordinates))
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
                                  [:circle {:fill :red } dot 1])
                                dots)))))))))

(defn to-csv
  [roads]
  (let [adjacency-list (atom {})]
    (do
      (doseq [road roads]
          (doseq [i (range (- (count (:nodes road)) 1))]
            (let [ith (nth (:nodes road) i)
                  i+1th (nth (:nodes road) (+ i 1))
                  ith-adjccs (ith @adjacency-list)
                  i+1th-adjcs (i+1th @adjacency-list)]
              (case (:direction road)
                :--> (if (nil? ith-adjccs)
                       (swap! adjacency-list
                              assoc ith [i+1th])
                       (swap! adjacency-list
                              assoc ith (conj ith-adjccs i+1th)))
                :<-- (if (nil? i+1th-adjcs)
                       (swap! adjacency-list
                              assoc i+1th [ith])
                       (swap! adjacency-list
                              assoc i+1th (conj ith-adjccs ith)))
                :<-> (do
                       (if (nil? ith-adjccs)
                         (swap! adjacency-list
                                assoc ith [i+1th])
                         (swap! adjacency-list
                                assoc ith (conj ith-adjccs i+1th)))
                       (if (nil? i+1th-adjcs)
                         (swap! adjacency-list
                                assoc i+1th [ith])
                         (swap! adjacency-list
                                assoc i+1th (conj ith-adjccs ith))))))))
      (with-open [writer (io/writer "adjacency-list.csv")]
        (csv/write-csv writer
                       (->> @adjacency-list
                            (apply list)
                            (map flatten)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (time
    (let [roads (-> ways
                    get-roads
                    filter-roads)]
      (do
        (-> roads
            prepare-image
            (dali/render-svg "visualisation.svg"))
        (-> roads
            to-csv)))))