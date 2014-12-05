(ns falx.core
  (:import (com.badlogic.gdx Gdx)
           (clojure.lang Agent))
  (:require [gdx-loopy.core :refer [loop! on-render-thread]]
            [gdx-2d.core :as g]
            [gdx-2d.cam :as cam]
            [gdx-2d.color :as color]
            [silc.core :refer :all]
            [clojure.core.memoize :as mem]
            [clojure.string :as str]
            [clojure.set :as set]
            [cheshire.core :as json]
            [clj-tuple :refer [tuple]]
            [falx
             [state :refer :all]
             [input :as input]
             [point :as pt]]))

(defn ind
  "Maps 2d co-ordinates to a one dimensional co-ordinate given the width of the rectangular plane."
  [x y width]
  (+ (* y width) x))

(defn assoc-with
  "Assocs a value given by some function of the map `f` using the key `k`."
  [m k f]
  (assoc m k (f m)))

(defn update
  "Like update-in but for one level of nesting"
  [m k f & args]
  (assoc m k (apply f (get m k) args)))

(defn filter-keys
  [m pred]
  (into {} (filter (comp pred first) m)))

(defn idiv
  "Like `/` but coerces its result to an int"
  [a b]
  (int (/ a b)))

(defn selected?
  [game e]
  (boolean (att game e :selected?)))

(defn entities-at-mouse
  [game]
  (with-many game {:map (:map game) :pos (:mouse-cell game)}))

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
  (if (command-hit? m :mod)
    cam-fast-speed
    cam-slow-speed))

(defn cam-shift
  [m]
  (* (:delta m 0) (cam-speed m)))

(defmethod apply-command :cam-up
  [m _]
  (update m :cam (fnil pt/+ [0 0]) [0 (cam-shift m)]))

(defmethod apply-command :cam-down
  [m _]
  (update m :cam (fnil pt/+ [0 0]) [0 (- (cam-shift m))]))

(defmethod apply-command :cam-left
  [m _]
  (update m :cam (fnil pt/+ [0 0]) [(- (cam-shift m)) 0]))

(defmethod apply-command :cam-right
  [m _]
  (update m :cam (fnil pt/+ [0 0]) [(cam-shift m) 0]))

(defn load-tiled-map
  "Load a tiled map from a file and return its contents as a map"
  [file]
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

(defn gdx-width
  []
  (. Gdx/graphics getWidth))

(defn gdx-height
  []
  (. Gdx/graphics getHeight))

(defn gdx-size
  []
  (tuple (gdx-width) (gdx-height)))

(def default-cell-size
  "The default cell size used by the game"
  [32 32])

(defn cell-size
  "Return the cell size used by the game"
  [m]
  (:cell-size m default-cell-size))

(defn mouse-cell
  "Return the pt x,y in the world
  that the mouse is currently over."
  [m]
  (let [[x y] (or (:mouse-world m) pt/id)
        [w h] (cell-size m)]
    (tuple (idiv x w)
           (idiv (- y h) (- h)))))

(defn update!
  "Called every frame"
  []
  (when (>= 100 (.getQueueCount ^Agent game))
    (await game))
  (let [fps (g/fps)
        delta (g/delta)
        screen (gdx-size)
        input* (swap! input input/next-state (input/input))
        mouse (:mouse-pos input*)
        world-mouse (if-let [cam @cam] (cam/unproject cam mouse) pt/id)
        commands (input/commands-hit input* input/default-key-bindings)]
    (send game #(-> (assoc %
                           :fps fps
                           :screen screen
                           :mouse-screen mouse
                           :mouse-world world-mouse
                           :delta delta)
                    (assoc-with :mouse-cell mouse-cell)
                    (apply-commands commands)))))

(def pprint-str #(with-out-str (clojure.pprint/pprint %)))

(def mem-pprint-str
  "A memoized version of pprint that returns a string"
  (mem/lru pprint-str :lru/threshold 10))

(def mem-filter-keys
  "A memoized version of filter keys"
  (mem/lru filter-keys :lru/threshold 10))

(defn debug-entity-attributes
  "Returns a debug string for the given entities attributes"
  [game e]
  (str
    "entity: " e
    "\n"
    (mem-pprint-str (mem-filter-keys (atts game e) keyword?))))

(defn debug
  "Returns a debug string for the given game state"
  [game]
  (str/join
    "\n"
    (concat [(select-keys game [:fps :screen :map])
             (select-keys game [:mouse-screen :mouse-world :mouse-cell])
             (str "commands: " (:commands game))
             ""]
            (map #(debug-entity-attributes game %) (entities-at-mouse game)))))

(defn screen
  [m]
  (:screen m default-size))

(def width "Get the current screen width" (comp first screen))
(def height "Get the current screen height" (comp second screen))

(defn bottom-left
  [_]
  [0 0])

(defn bottom-middle
  [m]
  [(/ (width m) 2) 0])

(defn bottom-right
  [m]
  [(width m) 0])

(defn middle
  [m]
  [(/ (width m) 2) (/ (height m) 2)])

(defn middle-left
  [m]
  [0 (/ (height m) 2)])

(defn middle-right
  [m]
  [(width m) (/ (height m) 2)])

(defn top-left
  [m]
  [0 (height m)])

(defn top-middle
  [m]
  [(/ (width m) 2) (height m)])

(defn top-right
  [m]
  [(width m) (height m)])

(defn draw-screen-positions!
  "Draws some markers to help position ui components"
  [game]
  (let [[x y] (top-left game)]
    (g/draw-text! "x" x y))
  (let [[x y] (top-middle game)]
    (g/draw-text! "x" x y))
  (let [[x y] (top-right game)]
    (g/draw-text! "x" (- x 10) y))
  (let [[x y] (middle game)]
    (g/draw-text! "x" x y))
  (let [[x y] (middle-left game)]
    (g/draw-text! "x" x y))
  (let [[x y] (middle-right game)]
    (g/draw-text! "x" (- x 10) y))
  (let [[x y] (bottom-left game)]
    (g/draw-text! "x" x (+ y 16)))
  (let [[x y] (bottom-middle game)]
    (g/draw-text! "x" x (+ y 16)))
  (let [[x y] (bottom-right game)]
    (g/draw-text! "x" (- x 10) (+ y 16))))

(defn draw-debug!
  "Draws some useful debug information to the screen"
  [game]
  (let [[x y] (top-left game)]
    (g/draw-text! (debug game) x y)))

(defn entities-in-layer
  "Return the set of entities in the map and layer"
  [m map layer]
  (with-many m {:map map :layer layer}))

(defn draw-basic-layer!
  "Draw a basic set of entities for the given map and layer
   this simply renders entities with a :sprite and :pos value"
  [game map* layer]
  (doseq [e (entities-in-layer game map* layer)
          :let [e (atts game e)]]
    (when-let [sprite (:sprite e)]
      (when-let [[x y] (:pos e)]
        (g/draw-point! sprite (* x 32) (* y -32))))))

(defn sync-camera!
  [game]
  (let [cam @cam]
    (when-let [[x y] (:cam game)]
      (cam/move! cam x y)
      (cam/update! cam))))

(defn draw-creature-circle!
  [game e x y]
  (when (selected? game e)
    (g/with-color color/green
      (g/draw-point! :selected x y))))

(defn draw-creature!
  [game e x y]
  (draw-creature-circle! game e x y)
  (when-let [sprite (att game e :sprite)]
    (g/draw-point! sprite x y)))

(defn draw-creature-layer!
  [game map*]
  (doseq [e (entities-in-layer game map* :creature)]
    (when-let [[x y] (att game e :pos)]
      (draw-creature! game e (* x 32) (* y -32)))))

(defn draw-map!
  [game]
  (when (:map game)
    (draw-basic-layer! game (:map game) :base)
    (draw-basic-layer! game (:map game) :decor)
    (draw-basic-layer! game (:map game) :object)
    (draw-creature-layer! game (:map game))))

(defn render!
  []
  (update!)
  (g/clear!)
  (let [game @game]
    (when-let [batch @batch]
      (g/with-batch batch
        (g/with-font @font
          (sync-camera! game)
          (g/with-camera @cam
            (try
              (draw-map! game)
              (catch Throwable e
                (.printStackTrace e))))
          (draw-screen-positions! game)
          (draw-debug! game))))))

(comment
  "begin ze game"
  (loop! #'render!
         (assoc settings
                :max-fps 0))
  "Init ze game"
  (init!)
  "load a map"
  (def example-map (load-tiled-map "test-resources/test-map.json"))
  "tiles from tile layers"
  (take 5 (mapcat #(tile-layer->tiles example-map %) (:layers example-map)))
  "tiles"
  (first (tiles example-map))
  (distinct (map :sprite (tiles example-map)))
  (first (filter #(= (:terrain %) :wall) (tiles example-map)))
  "objects"
  (first (objects example-map))
  "the map entity"
  (tiled-map-entity example-map)

  (do
    "load the example map into the game"
    (do (send game creates (concat (tiles example-map) (objects example-map)))
        nil)
    "load the tilemap entity itself into the game"
    (let [ent (tiled-map-entity example-map)]
      (send game set-atts (:name ent) ent)
      nil)
    "set the map to be the example map"
    (do (send game assoc :map :test-map) nil))

  "Move the camera"
  (do (swap! game assoc :cam [0 0]) nil)

  "reset the game to its default"
  (do (send game (constantly default-game))
      nil)
  "await the game"
  (await-for 1000 game))

