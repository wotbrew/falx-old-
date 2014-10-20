(ns falx.core
  (:import (com.badlogic.gdx.scenes.scene2d.utils Drawable)
           (com.badlogic.gdx.graphics.g2d TextureAtlas TextureRegion SpriteBatch))
  (:require [falx.util :refer :all]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.set :as set]
            [clj-tuple :refer [tuple]]
            [falx.io :as io]
            [falx.state :as state]))

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


;;helper

(defn next-id
  [m]
  (:id m 0))

(defn inc-id
  [m]
  (update m :id (fnil inc 0)))

(defn log
  [m & words]
  (update m :log vec-conj (str/join " " words)))

;;attributes

(defn attr
  ([m id att]
   (-> m :attr (get id) (get att)))
  ([m id att else]
   (-> m :attr (get id) (get att else))))

(defn attrs
  [m id]
  (-> m :attr (get id)))

(defn by-attr
  [m att value]
  (-> m :attr-ave (get att) (get value)))

(defn attrfn
  [att]
  (fn
    ([m id else] (attr m id att else))
    ([m id] (attr m id att))))

(defmacro defattr
  ([sym doc att]
     `(def ~sym ~doc (attrfn ~att)))
  ([sym doc]
     `(defattr ~sym ~doc ~(keyword (name sym))))
  ([sym]
     `(defattr ~sym ~(str "attribute " (name sym)))))

(defn by-attrfn
  [att]
  (fn [m value] (by-attr m att value)))

(defmacro defby-attr
  ([sym doc att]
     `(def ~sym ~doc (by-attrfn ~att))))

(defn flagfn
  [att]
  (fn [m] (by-attr m att true)))

(defn with-attr
  [m att]
  (-> m :attr-ae (get att)))

(def ave? #{:pos :world :layer :type :player?})

(def ae? #{:sprite :image})

(def vae? #{})

(defn index-ave
  [m id att value]
  (update-in m [:attr-ave att value] set-conj id))

(defn unindex-ave
  [m id att value]
  (let [new-set (disj (-> m :attr-ave (get att) (get value)) id)]
    (if (empty? new-set)
      (dissoc-in m [:attr-ave att value])
      (assoc-in m [:attr-ave att value] new-set))))

(defn index-vae
  [m id att value]
  (update-in m [:attr-vae value att] set-conj id))

(defn unindex-vae
  [m id att value]
  (let [new-set (disj (-> m :attr-vae (get value) (get att)) id)]
    (if (empty? new-set)
      (dissoc-in m [:attr-vae value att])
      (assoc-in m [:attr-vae value att] new-set))))

(defn unindex-ae
  [m id att]
  (let [new-set (disj (-> m :attr-ae (get att)) id)]
    (if (empty? new-set)
      (dissoc-in m [:attr-ae att])
      (assoc-in m [:attr-ae att] new-set))))

(defn index-ae
  [m id att]
  (update-in m [:attr-ae att] set-conj id))

(def attr-explodes?
  (-> {:pos {:world first
             :pt pos->pt}}
      (map-vals juxt-map)))

(declare rem-attrs)

(defn rem-attr
  [m id att]
  (let [value (attr m id att)
        explodes (attr-explodes? att)]
    (cond-> (dissoc-in m [:attr id att])
      (ave? att) (unindex-ave id att value)
      (vae? att) (unindex-vae id att value)
      (ae? att) (unindex-ae id att)
      explodes (rem-attrs id (explodes value)))))

(defn rem-attrs
  [m id kvs]
  (reduce (fn [m k] (rem-attr m id k)) m kvs))

(declare set-attrs)

(defn set-attr
  [m id att value]
  (if (nil? value)
    (rem-attr m id att)
    (let [explodes (attr-explodes? att)]
      (cond-> (assoc-in m [:attr id att] value)
        (ave? att) (index-ave id att value)
        (vae? att) (index-vae id att value)
        (ae? att) (index-ae id att)
        explodes (set-attrs id (explodes value))))))

(defn set-attrs
  [m id kvs]
  (reduce (fn [m [k v]] (set-attr m id k v)) m kvs))

(defn update-attr
  [m id att f & args]
  (let [value (attr m id att)]
    (set-attr m id att (apply f value args))))

(defn update-attrs
  [m att f & args]
  (reduce #(apply update-attr %1 %2 att f args) m (with-attr m att)))

(defn- create*
  [m attrs]
  (let [id (next-id m)
        m (inc-id m)]
    (-> (set-attr m id :id id)
        (set-attrs id attrs))))

(defmulti create (fn [m attrs] (:type attrs)))

(defmethod create :default
  [m attrs]
  (create* m attrs))

(defmethod create :creature
  [m attrs]
  (create* m (assoc attrs
               :solid? true)))

(defn create-pair
  [m attrs]
  [(create m attrs) (next-id m)])


(defn create-world
  [m tiled-map]
  (let [id (next-id m)
        m (create m (select-keys tiled-map [:name :size]))
        objs (map-stream tiled-map)
        objs (for [o objs]
               (-> (assoc-in o [:pos 0] id)
                   (assoc-in [:layer 0] id)))]
    (reduce create m objs)))


(comment
  "Create the initial map like this"
  (def tmap (load-map "test-resources/test-map.json"))

  (def tgame (create-world nil tmap))
  "Create regions for all the :image entities"
  (def tgame (update-attrs tgame :image state/image->region))
  "Create regions for all the :sprite entities"
  (def tgame (update-attr tgame :sprite state/sprite->region)))

;;query

(defattr world "Get the entities world attribute")

(defby-attr by-world "Get all the entities existing on the given world" :world)

(defattr pos "Get the pos triple for the given id")

(defn positions
  "Get all the positions (pos triples) for the given world id `wid`"
  [m wid]
  (let [[w h] (attr m :size wid)]
    (for [x (range w)
          y (range h)]
      (tuple wid x y))))

(defattr pt "Get the pt (x, y) for the given id")

(defby-attr at "Get the set of entities at the position" :pos)

(defattr solid? "Is the entity solid?")

(defattr player? "Is the entity a player?")

(defn type=
  [m id type]
  (= type (attr m id :type)))

(defn creature?
  [m id]
  (type= m id :creature))

(def players (flagfn :player?))

;;commands

(defmulti apply-command (fn [m command] command))

(defmethod apply-command :default
  [m _]
  m)

(defn apply-commands
  [m commands]
  (let [m (assoc m :commands (set commands))]
    (reduce apply-command m commands)))


(defn command-hit?
  [m command]
  (-> m :commands (get command)))

(def cam-slow-speed 500)

(def cam-fast-speed (* 2.5 cam-slow-speed))

(defn cam-speed
  [m]
  (if (command-hit? m :cam-fast) cam-fast-speed
                                 cam-slow-speed))

(defn cam-shift
  [m]
  (* (:delta m 0) (cam-speed m)))

(defmethod apply-command :cam-up
  [m _]
  (update m :cam shift 0 (cam-shift m)))

(defmethod apply-command :cam-down
  [m _]
  (update m :cam shift 0 (- (cam-shift m))))

(defmethod apply-command :cam-left
  [m _]
  (update m :cam shift (- (cam-shift m)) 0))

(defmethod apply-command :cam-right
  [m _]
  (update m :cam shift (cam-shift m) 0))
