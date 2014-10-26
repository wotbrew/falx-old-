(ns falx.tiled
  (:require [falx.util :refer :all]
            [falx.game :as game]
            [falx.template :as templ]
            [falx.io :as io]
            [falx.db :as db]
            [clj-tuple :refer [tuple]]
            [clojure.set :as set]
            [clojure.string :as str]))


;;tiled-map

(defn name-map
  [m file]
  (assoc m :name (keyword (str/replace file #"(.+/)|(\.(.+))" ""))))

(defn size-map
  [m]
  (assoc m :size [(:width m) (:height m)]))

(defn tileset->tiles
  [tileset]
  (let [{:keys [terrains firstgid image imagewidth imageheight tilewidth tileheight]} tileset]
    (for [x (range 0 imagewidth tilewidth)
          y (range 0 imageheight tileheight)
          :let [pos (tuple x y tilewidth tileheight)
                x (int (/ x tilewidth))
                y (int (/ y tileheight))
                id (ind x y (/ imagewidth tilewidth))
                tile (-> tileset :tiles (get (keyword (str id))))
                terrain (when tile (-> tile :terrain first terrains :name keyword))
                id (+ id firstgid)]]
      {:tid     id
       :image   {:file image
                 :rect pos}
       :terrain terrain})))

(defn tile-layer->tiles
  [layer m]
  (let [width (:width layer)
        height (:height layer)]
    (for [x (range width)
          y (range height)]
      {:pos (tuple (:name m) x y)
       :layer (tuple (:name m) (keyword (:name layer)))
       :tid (-> layer :data (get (ind x y width)))})))

(defn obj-layer->tiles
  [layer m]
  (let [{:keys [tilewidth tileheight]} m]
    (for [o (:objects layer)
          :let [{:keys [x y width height]} o
                [x y width height] (div-rect [x y width height] tilewidth tileheight)
                props (map-vals (:properties o) read-string)]
          x (range x (+ x width))
          y (range y (+ y height))]
      (merge {:pos   (tuple (:name m) x y)
              :name  (:name o)
              :type  (read-string (:type o))
              :layer (tuple (:name m) (keyword (:name layer)))}
             props))))

(defn tiles
  [m]
  (let [tilesets (mapcat tileset->tiles (:tilesets m))]
    (set/join
      tilesets
      (mapcat #(tile-layer->tiles % m) (:layers m)))))

(defn objects
  [m]
  (mapcat #(obj-layer->tiles % m) (:layers m)))

(defn load-map
  "Loads a tiled map from a file"
  [file-name]
  (-> (io/json file-name)
      (name-map file-name)
      size-map))

(defn map-stream 
  [m]
  (concat (tiles m) (objects m)))

(comment
  "load a map"
  (def tmap (load-map "test-resources/test-map.json"))

  "get tiles like this"
  (:layers tmap)
  (def ttiles (tiles tmap))
  (first ttiles)

  (def tobjects (objects tmap))

  (first tobjects)

  "you can create a stream of all the objects"
  (def tstream (map-stream tmap)))

(defn create-world
  "Create a world from a tiled map object"
  [m tiled-map]
  (let [[id m] (db/create m (select-keys tiled-map [:name :size]))
        objs (map-stream tiled-map)
        objs (for [o objs]
               (-> (assoc-in o [:pos 0] id)
                   (assoc-in [:layer 0] id)))]
    (db/creates m (map #(templ/apply-template % (:type %)) objs))))
