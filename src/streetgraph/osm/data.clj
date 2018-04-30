(ns streetgraph.osm.data
  ;; CONTAINS PARSED AND PROCESSED OSM DATA
  (:require [clojure.java.io :as io :refer [resource file reader]]
            [clojure.xml :as xml :refer [parse]]
            [clojure.zip :as zip :refer [xml-zip]]
            [clojure.data.zip.xml :refer [xml-> attr attr=]]
            [taoensso.nippy :refer [freeze-to-file]]
            [streetgraph.osm.parsing :refer :all]
            [streetgraph.osm.processing :refer :all]))

(def osm-file (-> "arkhangelsk.xml" io/resource io/file xml/parse zip/xml-zip))

(def ways (xml-> osm-file :way))

(def bounds
  (let [bounds-tag (first (doall (xml-> osm-file :bounds)))]
    {:min {:lat (read-string (attr bounds-tag :minlat))
           :lon (read-string (attr bounds-tag :minlon))}
     :max {:lat (read-string (attr bounds-tag :maxlat))
           :lon (read-string (attr bounds-tag :maxlon))}}))

(def mapping-props
  (let [x 2500
        lat-diff (- (get-in bounds [:max :lat])
                    (get-in bounds [:min :lat]))
        lon-diff (- (get-in bounds [:max :lon])
                    (get-in bounds [:min :lon]))
        km-in-lat 111.111
        km-in-lon (* km-in-lat (Math/cos ^double (-> bounds :min :lat (* Math/PI) (/ 180))))
        x:y ^double (/ (* lon-diff km-in-lon)
                       (* lat-diff km-in-lat))
        km-in-x (/ km-in-lon x)
        y (Math/round ^double (/ x x:y))]
    {:img-size {:x x :y y}
     :bounds   bounds
     :km       km-in-x}))

(def spher->cart
  (set-mapping bounds (:img-size mapping-props)))

(def roads-db
  (-> ways
      get-roads
      filter-roads))

(def road-coords-db
  (-> osm-file
      (get-coords spher->cart)
      (filter-coords roads-db)))

(def facilities
  (let [raw-facil (get-facilities osm-file spher->cart "hospital")]
    (into {}
          (for [f raw-facil
                :let [facil-coords (val f)]]
            [(key f) (merge facil-coords (get-closest-node facil-coords road-coords-db))]))))

(defn serialize-to
  [where-to]
  (do
    (freeze-to-file (str where-to "/roads.npy") roads-db)
    (freeze-to-file (str where-to "/adj_list.npy") (adj-list roads-db road-coords-db))
    (freeze-to-file (str where-to "/nodes.npy") road-coords-db)
    (freeze-to-file (str where-to "/facils.npy") facilities)
    (freeze-to-file (str where-to "/mapping_props.npy") mapping-props)))