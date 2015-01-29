(ns falx.tiled
  (:require [cheshire.core :as json]
            [clj-tuple :refer [tuple]]
            [falx.base :refer :all]
            [clojure.tools.logging :refer [info]]
            [clojure.string :as str]
            [clojure.set :as set]))


(defn load-tiled-map
  "Load a tiled map from a file and return its contents as a map"
  [file]
  (info "Loading tiled map" file)
  (-> (json/parse-string (slurp file) true)
      (assoc :name (keyword (str/replace file #"(.+/)|(\.(.+))" "")))
      (assoc-with :size (juxt :width :height))))

(defn atlas-file
  "Translates a filename into one that will be accepted by texture atlases"
  [file]
  (second (re-find #"\/(.+)\." file)))

(defn tileset->tiles
  "Transform a tiled map tileset into a seq of tiles"
  [tileset]
  (let [{:keys [terrains firstgid image imagewidth imageheight tilewidth tileheight]} tileset]
    (for [x (range 0 imagewidth tilewidth)
          y (range 0 imageheight tileheight)
          :let [pos (tuple x y tilewidth tileheight)
                x (idiv x tilewidth)
                y (idiv y tileheight)
                id (ind x y (/ imagewidth tilewidth))
                tile (-> tileset :tiles (get (keyword (str id))))
                terrain (when tile (-> tile :terrain first terrains :name keyword))
                id (+ id firstgid)]]
      {:tid id
       :sprite [(atlas-file image) pos]
       :terrain terrain})))

(defn tile-layer->tiles
  "Transform a tiled map layer into a seq of tiles"
  [tiled layer]
  (let [{:keys [width height name data]} layer
        map-name (:name tiled)]
    (for [x (range width)
          y (range height)]
      {:pos (tuple x y)
       :layer (keyword name)
       :map map-name
       :tid (get data (ind x y width))})))

(defn tiles
  "Return a seq of all the tiles in a tiled map.
   Joins tileset tiles and layer tiles together"
  [tiled]
  (let [tilesets (mapcat tileset->tiles (:tilesets tiled))]
    (set/join
      tilesets
      (mapcat #(tile-layer->tiles tiled %) (:layers tiled)))))

(defn obj-layer->objects
  "Transform a tiled map object layer into a seq of objects"
  [tiled layer]
  (let [{:keys [tilewidth tileheight]} tiled]
    (for [o (:objects layer)
          :let [{:keys [x y width height]} o
                [x y width height] [(idiv x tilewidth) (idiv y tileheight)
                                    (idiv width tilewidth) (idiv height tileheight)]
                props (into {} (map (fn [[k v]] [k (read-string v)]) (:properties o)))]
          x (range x (+ x width))
          y (range y (+ y height))]
      (merge {:pos   (tuple x y)
              :map   (:name tiled)
              :name  (:name o)
              :type  (read-string (:type o))
              :layer (keyword (:name layer))}
             props))))

(defn objects
  "Return a seq of all the objects in a tiled map"
  [tiled]
  (mapcat #(obj-layer->objects tiled %) (:layers tiled)))

(defn tiled-map-entity
  "Returns an entity map representing the tiledmap itself"
  [m]
  (select-keys m [:width :height :name]))
