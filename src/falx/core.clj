(ns falx.core
  (:import (com.badlogic.gdx Gdx)
           (clojure.lang Agent))
  (:require [gdx-loopy.core :refer [loop! on-render-thread]]
            [gdx-2d.core :as g]
            [gdx-2d.cam :as cam]
            [gdx-2d.color :as color]
            [silc.core :refer :all]
            [clojure.tools.logging :refer [info debug error]]
            [clojure.core.memoize :as mem]
            [clojure.string :as str]
            [clj-tuple :refer [tuple]]
            [falx
             [base :refer :all]
             [tiled :refer :all]
             [state :refer :all :as state]
             [lifecycle :refer :all]
             [input :as input]
             [point :as pt]]
            [falx.proc
             [ai :as ai]
             [ui-pather :as ui-pather]]))


;;tiles


(defn gdx-width
  []
  (. Gdx/graphics getWidth))

(defn gdx-height
  []
  (. Gdx/graphics getHeight))

(defn gdx-size
  []
  (tuple (gdx-width) (gdx-height)))


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
                    (apply-commands commands)
                    simulate))))

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
             (str "in-game? " (mouse-in-game? game))
             ""]
            (map #(debug-entity-attributes game %) (at-mouse game)))))

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
  (let [[x] (game-buffer game)
        [_ sh] (screen game)]
    (g/draw-text! (debug-str game) x sh)))

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
        (draw-creature-circle! game e x y)
        (g/with-color (if (att game e :hit-time)
                        color/red
                        color/white)
          (draw-creature! game e x y))))))

(defn draw-map!
  [game]
  (when (:map game)
    (let [[cw ch] (cell-size game)]
      (draw-basic-layer! game (:map game) :base cw ch)
      (draw-basic-layer! game (:map game) :decor cw ch)
      (draw-basic-layer! game (:map game) :object cw ch)
      (draw-creature-layer! game (:map game) cw ch))))

(defn draw-world-texts!
  [game]
  (let [[cw ch] (cell-size game)]
    (doseq [world-text (:world-text game)]
      (g/with-font-color (:color world-text)
        (when-let [[x y] (:pos world-text)]
          (let [y (- (* y (- ch)) -64 (max (:time world-text) 15))]
            (g/draw-text! (:text world-text) (* cw x) y)))))))

(defn draw-ui-path!
  [game]
  (when-let [ui-path (:ui-path game)]
    (let [[cw ch] (cell-size game)]
      (doseq [[x y] (rest ui-path)]
        (g/draw-point! :yellow-flag (* x cw) (- (* y ch)))))))

(defn draw-world!
  [game]
  (draw-map! game)
  (draw-ui-path! game)
  (draw-world-texts! game))

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

(defn draw-blanks!
  ([x y w h]
   (draw-blanks! x y w h 32 32))
  ([x y w h cw ch]
   (dotimes [xx (idiv w cw)]
     (dotimes [yy (idiv h ch)]
       (let [x (+ x (* xx cw))
             y (+ y (* yy ch))]
         (g/draw-point! :blank x y))))))

(defn draw-black!
  [x y w h]
  (g/with-color color/black
    (g/draw-quad! @state/pixel x y w h)))

(defn draw-buffers!
  [game]
  (let [[x y w h] (left-buffer game)]
    (draw-black! x y w h)
    (draw-blanks! w 0 32 h 32 32))
  (let [[x y w h] (right-buffer game)]
    (draw-black! x y w h)
    (draw-blanks! (- x 32) 0 32 h 32 32))
  (let [[x y w h] (bottom-buffer game)]
    (draw-black! x y w h)
    (draw-blanks! 0 h w 32 32 32)))

(defn draw-box!
  ([color x y w h]
   (g/with-color color
     (draw-box! x y w h)))
  ([x y w h]
   (let [p @state/pixel]
     (g/draw-quad! p x y 1 h)
     (g/draw-quad! p (+ x w -1) y 1 h)
     (g/draw-quad! p x y w 1)
     (g/draw-quad! p x (+ y h -1) w 1))))

(defn draw-player-backing!
  [game player x y w h]
  (let [color (cond
                (mouse-in? game x y w h) color/yellow
                (selected? game player) color/green
                :else color/white)]
    (draw-box! color x y w h)))

(defn draw-player!
  [game player buffer]
  (let [[x y w h] buffer]
    (draw-player-backing! game player x y w h)
    (when-let [spr (att game player :sprite)]
      (g/draw-text! (str "eid " player) (+ x 8) (+ y h -8))
      (g/draw-quad! spr (+ x 32) (+ y h -96) 64 64))))

(defn draw-players!
  [game]
  (dotimes [n 6]
    (when-let [player (player game n)]
      (draw-player! game player (player-buffer game n)))))

(defn draw-ui!
  [game]
  (draw-buffers! game)
  (draw-players! game)
  (draw-screen-positions! game)
  (draw-debug! game)
  (draw-mouse! game))

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
              (draw-world! game)
              (catch Throwable e
                (.printStackTrace e))))
          (draw-ui! game))))))

;;examples

(comment
  "begin ze game"
  (loop! #'render!
         (assoc settings
                :max-fps 60))
  "Init ze game"
  (init!)


  (def bs (ai/brain-spawner))
  (start bs)
  (stop bs)

  (def ui-pather (ui-pather/ui-pather))
  (start ui-pather)
  (stop ui-pather)

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

