(ns falx.core
  (:import (com.badlogic.gdx Gdx)
           (clojure.lang Agent))
  (:require [gdx-loopy.core :refer [loop! on-render-thread]]
            [gdx-2d.core :as g]
            [gdx-2d.cam :as cam]
            [gdx-2d.color :as color]
            [silc.core :refer :all]
            [clj-tiny-astar.path :refer [a*]]
            [clojure.tools.logging :refer [info debug error]]
            [clojure.core.memoize :as mem]
            [clojure.core.async :refer [<! >! chan go go-loop timeout] :as async]
            [clojure.string :as str]
            [clojure.set :as set]
            [cheshire.core :as json]
            [clj-tuple :refer [tuple]]
            [falx
             [base :refer :all]
             [state :refer :all :as state]
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

(defn ffilter
  [pred coll]
  (first (filter pred coll)))

(defn idiv
  "Like `/` but coerces its result to an int"
  [a b]
  (int (/ a b)))

(defmulti apply-command (fn [m command] command))

(defmethod apply-command :default
  [m _]
  m)

(defn command-hit?
  [m command]
  (-> m :commands (get command)))

(defn mod?
  [m]
  (command-hit? m :mod))

(defn apply-commands
  [m commands]
  (let [m (assoc m :commands (set commands))]
    (reduce apply-command m commands)))

(defn mouse
  "Returns the current mouse position in terms of world cells."
  [game]
  (:mouse-cell game pt/id))

(defn pos
  "Returns the position of the entity"
  [m e]
  (att m e :pos))

(defn at
  "Find the entities at the given point (and map)"
  ([game pt]
   (at game (:map game) pt))
  ([game map pt]
   (with-many game {:map map :pos pt})))

(defn at-mouse
  "Find the entities at the mouse position"
  [game]
  (at game (mouse game)))

(defn at-fn
  "Returns a function that when passed a game
   and pt will return the entities at
   the point that meet the predicate"
  [pred]
  (fn
    ([game map pt]
     (filter #(pred game %) (at game map pt)))
    ([game pt]
     (filter #(pred game %) (at game pt)))))

(defn at-mouse-fn
  "Returns a function that when passed a game
   will return the entities at the mouse
   that meet the predicate"
  [pred]
  (fn [game]
    (filter #(pred game %) (at-mouse game))))

(defn att-fn
  "Returns a function that when passed a game
   and entity will return the att `k`."
  [k]
  (fn [game e] (att game e k)))

(defn type-is-fn
  "Returns a function that when passed a game
   and entity will return whether the type is equal to that
   which was used to create a function"
  [type]
  (fn [game e] (= type (att game e :type))))

(def creature?
  "Is the entity a creature?"
  (type-is-fn :creature))

(defn creatures
  "Returns all the creatures"
  [game]
  (with game :type :creature))

(def creature-at-mouse
  "Return the entities at the mouse position"
  (comp first (at-mouse-fn creature?)))

(defn player?
  [m e]
  (att m e :player?))

(defn enemy?
  [m e]
  (att m e :enemy?))

(def selected?
  "Is the entity selected?"
  (att-fn :selected?))

(defn selected
  "Returns all the selected entities"
  [m]
  (all m :selected?))

(defn unselect
  "Unselects the entity"
  [m e]
  (delete-att m e :selected?))

(defn unselect-all
  "Unselects any entity which is selected"
  [m]
  (reduce unselect m (selected m)))

(defn selectable?
  "Is the given entity selectable"
  [m e]
  (and (creature? m e)
       (player? m e)))

(def selectable-at
  "Get the selectable entity at the given point or nil"
  (at-fn selectable?))

(def selectable-at?
  "Is (any) entity at the given point selectable?"
  (comp boolean first selectable-at))

(def selectable-at-mouse
  "Gets the selectable entity at the mouse position
   or nil"
  (comp first (at-mouse-fn selectable?)))

(defn select
  "Selects the entity"
  [m e]
  (if (selectable? m e)
    (set-att m e :selected? true)
    m))

(defn select-only
  "Selects only the entity supplied, unselecting all other entities"
  [m e]
  (-> (unselect-all m)
      (select e)))

(defn perform-select
  "Selects the entity
   - if the modifier key is down it is added to the selection
   - otherwise only the given selection is made"
  [m e]
  (if (mod? m)
    (select m e)
    (select-only m e)))

(defn select-at-mouse
  "Selects the entity at the mouse"
  [m]
  (if-let [e (creature-at-mouse m)]
    (perform-select m e)
    m))

(defn solid?
  "Is the entity solid?"
  [m e]
  (or (att m e :solid?)
      (= :wall (att m e :terrain))
      (creature? m e)))

(def solid-at
  "Returns a seq of solid entities at the given point"
  (at-fn solid?))

(def solid-at?
  (comp boolean first solid-at))

(defn goto
  "LOL - doesn't perform a goto.
   Rather adds the intention to goto the given point."
  [m e pt]
  (set-att m e :goto pt))

(defn forget-goto
  "Clears the goto intention from the entity"
  [m e]
  (delete-att m e :goto))

(defn selected-goto
  "Instructs the selected entities to goto the given pt"
  [m pt]
  (reduce #(goto %1 %2 pt) m (selected m)))

(defn selected-goto-mouse
  "Instructs the selected entities to goto the mouse position"
  [m]
  (selected-goto m (mouse m)))

(defn dead?
  "Is the entity dead?"
  [m e]
  (att m e :dead?))

(defn map-size
  "Returns the size of the given map as a tuple"
  [m map]
  [(att m map :width) (att m map :height)])

(defn adjacent-to?
  "Is the entity adjacent to the given point?"
  [m e pt]
  (when-let [pos (pos m e)]
    (pt/adj? pos pt)))

(defn adjacent?
  "Are the 2 entities adjacent to each other?"
  [m a b]
  (when-let [pos (pos m b)]
    (adjacent-to? m a pos)))

(defn can-move?
  "Can the entity move to the point
   - is it possible?"
  [m e pt]
  (and
    (not (solid-at? m pt))
    (adjacent-to? m e pt)))

(defn move
  "Attempt to move the entity from its current position
   to the one specified"
  [m e pt]
  (if (can-move? m e pt)
    (set-att m e :pos pt)
    m))

(defn can-attack?
  "Can the given entity `a` attack the other one `b`
   - is it possible?"
  [m a b]
  (and
    (not= a b)
    (creature? m a)
    (creature? m b)
    (adjacent? m a b)))

(defn attackable?
  "Is the given entity attackable by the (first) selected entity"
  [m e]
  (and
    (enemy? m e)
    (can-attack? m e (first (selected m)))))

(def attackable-at-mouse
  "Get the attackable entity at the mouse position if possible or nil"
  (comp first (at-mouse-fn attackable?)))

(defn attack-offset
  "Returns the offset point to use when
   entity a attacks entity b"
  [m a b]
  (let [pa (pos m a)
        pb (pos m b)]
    (pt/explode (pt/direction pa pb) 8 -8)))

(defn attack-at-mouse
  [m]
  (let [e (first (selected m))
        target (creature-at-mouse m)]
    (if (can-attack? m e target)
      (set-att m e :offset (attack-offset m e target))
      m)))

;;commands

(def cam-slow-speed
  500)

(def cam-fast-speed
  (* 2.5 cam-slow-speed))

(defn cam-speed
  [m]
  (if (mod? m)
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

(defmethod apply-command :primary
  [m _]
  (cond
    (selectable-at-mouse m) (select-at-mouse m)
    (attackable-at-mouse m) (attack-at-mouse m)
    :else (selected-goto-mouse m)))

;;brain
(def brain-tick 100)
(def brain-spawner-tick 500)
(def walk-tick 125)
(def animate-tick 50)

(defn path
  [game e to]
  (go
    (let [pos (pos game e)
          map (att game e :map)
          bounds (map-size game map)
          pred #(or (= pos %) (not (solid-at? game %)))]
      (when (and pos map bounds)
        (a* bounds pred pos to)))))

(declare bwalk!)

(defn forget-goto!
  [e]
  (send state/game forget-goto e))

(defn attempt-move!
  [e next]
  (go (let [game @game
            dead? (dead? game e)]
        (if dead?
          (do (debug e "is dead - cancelling move")
              (forget-goto! e))
          (do (send state/game move e next)
              (await state/game)
              (<! (timeout walk-tick)))))))

(defn attempt-walk!
  [e path]
  (let [target (last path)]
    (go-loop
      [[p & rest] path]
      (cond
        (not p) (debug e "is done walking")
        (not= target (att @game e :goto)) (do
                                            (debug e "goto target changed - repathing")
                                            (<! (bwalk! e)))
        :else (do
                (debug e "moving to" p)
                (<! (attempt-move! e p))
                (if (not= (pos @game e) p)
                  (do
                    (debug e "could not move due to unforseen obstacle - repathing")
                    (<! (bwalk! e)))
                  (recur rest)))))))

(defn bwalk-at-goal!
  [e]
  (debug e "is at its goal")
  (send state/game forget-goto e)
  (await state/game))

(defn bwalk-goto-now-solid!
  [goto e]
  (debug e "can no longer move to" goto "- it is solid")
  (forget-goto! e))

(defn bwalk!
  [e]
  (go
    (let [game @game
          goto (att game e :goto)]
      (cond
        (not goto) true
        (= goto (pos game e)) (bwalk-at-goal! e)
        (solid-at? game goto) (bwalk-goto-now-solid! goto e)
        :else (if-let [pa (<! (path game e goto))]
                (do
                  (debug e "path to" goto "is" pa)
                  (<! (attempt-walk! e pa)))
                (forget-goto! e))))))

(defn unoffset!
  [e offset]
  (go-loop
    [offset offset]
    (if-not (= offset pt/id)
      (do
        (send game set-att e :offset
              (pt/intify (pt/+ offset (pt/direction offset pt/id))))
        (<! (timeout animate-tick))
        (recur (att @game e :offset pt/id)))
      (do
        (debug e "offset undone")
        (send game delete-att e :offset)))))

(defn banimate!
  [e]
  (go
    (when-let [offset (att @game e :offset)]
      (debug e "undoing offset")
      (<! (unoffset! e offset)))))

(defn do-brain!
  "Performed on each brain tick"
  [e]
  (go
    (<! (bwalk! e))
    (<! (banimate! e))))

(defn do-brain-spawning!
  []
  (let [creatures (creatures @game)
        new
        (dosync
          (let [new (set/difference creatures @conscious)]
            (commute conscious into new)
            new))]
    (doseq [e new]
      (debug e "now has a brain")
      (go-loop
        []
        (let [game @game
              dead? (or (dead? game e) (not (creature? game e)))]
          (when dead?
            (debug e "is dead - removing brain")
            (dosync (commute conscious disj e)))
          (when-not dead?
            (try
              (<! (do-brain! e))
              (catch Throwable e
                (.printStackTrace e)
                (<! (timeout 1000))))
            (<! (timeout brain-tick))
            (recur)))))))

(defrecord BrainSpawner
  [kill]
  ILifecycle
  (start [this]
    (info "Starting brain spawner")
    (reset! kill false)
    (go-loop []
      (if-not @kill
        (do
          (do-brain-spawning!)
          (<! (timeout brain-spawner-tick))
          (recur))
        (info "Stopped brain spawner")))
    this)
  (stop [this]
    (info "Stopping brain spawner")
    (reset! kill true)
    this))

(defn brain-spawner
  "A brain spawner is a ILifecycle component
   that will when started periodically start ai brains
   for entities where appropriate."
  []
  (->BrainSpawner (atom false)))


;;tiles

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

;;debug

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

(defn debug-str
  "Returns a debug string for the given game state"
  [game]
  (str/join
    "\n"
    (concat [(select-keys game [:fps :screen :map])
             (select-keys game [:mouse-screen :mouse-world :mouse-cell])
             (str "commands: " (:commands game))
             ""]
            (map #(debug-entity-attributes game %) (at-mouse game)))))

;;screen

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

;;rendering

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
    (g/draw-text! (debug-str game) x y)))

(defn entities-in-layer
  "Return the set of entities in the map and layer"
  [m map layer]
  (with-many m {:map map :layer layer}))

(defn draw-basic-layer!
  "Draw a basic set of entities for the given map and layer
   this simply renders entities with a :sprite and :pos value"
  [game map* layer cw ch]
  (doseq [e (entities-in-layer game map* layer)
          :let [e (atts game e)]]
    (when-let [sprite (:sprite e)]
      (when-let [[x y] (:pos e)]
        (g/draw-point! sprite (* x cw) (* y (- ch)))))))

(defn sync-camera!
  [game]
  (let [cam @cam]
    (when-let [[x y] (:cam game)]
      (cam/move! cam x y)
      (cam/update! cam))))

(defn draw-creature-circle!
  [game e x y]
  (when (enemy? game e)
    (g/with-color color/red
      (g/draw-point! :selected x y)))
  (when (selected? game e)
    (g/with-color color/green
      (g/draw-point! :selected x y))))

(defn draw-creature!
  [game e x y]
  (draw-creature-circle! game e x y)
  (let [[xo yo] (att game e :offset pt/id)
        x (+ xo x)
        y (+ yo y)]
    (when-let [sprite (att game e :sprite)]
      (g/draw-point! sprite x y))))

(defn draw-creature-layer!
  [game map* cw ch]
  (doseq [e (entities-in-layer game map* :creature)]
    (when-let [[x y] (att game e :pos)]
      (let [x (* x cw)
            y (* y (- ch))]
        (draw-creature! game e x y)))))

(defn draw-map!
  [game]
  (when (:map game)
    (let [[cw ch] (cell-size game)]
      (draw-basic-layer! game (:map game) :base cw ch)
      (draw-basic-layer! game (:map game) :decor cw ch)
      (draw-basic-layer! game (:map game) :object cw ch)
      (draw-creature-layer! game (:map game) cw ch))))

(defn mouse-sprite
  [game]
  (cond
    (attackable-at-mouse game) :mouse-attack
    (selectable-at-mouse game) :mouse-select
    :else :mouse))

(defn draw-mouse!
  [game]
  (let [[x y] (:mouse-screen game)
        [_ h] (screen game)
        [_ cs] (cell-size game)]
    (when (and x y)
      (let [sprite (or (mouse-sprite game) :mouse)]
        (g/draw-point! sprite x (- h y cs))))))

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
          (draw-debug! game)
          (draw-mouse! game))))))

;;examples

(comment
  "begin ze game"
  (loop! #'render!
         (assoc settings
                :max-fps 60))
  "Init ze game"
  (init!)
  (def bs (brain-spawner))
  (start bs)
  (stop bs)
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


  "reset the game to its default"
  (do (restart-agent game default-game)
      nil)
  (send game (constantly default-game))
  "await the game"
  (await-for 1000 game))

