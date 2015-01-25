(ns falx.core
  (:import (com.badlogic.gdx Gdx)
           (clojure.lang Agent))
  (:require [gdx-loopy.core :refer [loop! on-render-thread]]
            [gdx-2d.core :as g]
            [gdx-2d.cam :as cam]
            [gdx-2d.color :as color]
            [silc.core :refer :all]
            [clojure.tools.logging :refer [info debug error]]
            [clj-tuple :refer [tuple]]
            [falx
             [base :refer :all]
             [tiled :refer :all]
             [state :refer :all]
             [lifecycle :refer :all]
             [input :as input]
             [point :as pt]]
            [falx.proc
             [ai :as ai]
             [ui-pather :as ui-pather]
             [turn-ender :as turn-ender]]
            [falx.ui
             [main :as ui-main]
             [shared :refer :all]]))

;;top level click handler

(defmethod apply-command :primary
  [m _]
  (if (ui-main/mouse-in-game? m)
    (handle-primary-in-game m)
    (ui-main/handle-primary m)))


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
                           :delta delta
                           :elapsed (+ delta (:elapsed % 0)))
                    (assoc-with :mouse-cell mouse-cell)
                    (apply-commands commands)
                    simulate))))



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

(def entities-in-layer-key
  #{:map :layer})

(def memmlyr (mem2 (fn [m l] {:map m :layer l})))

(defn entities-in-layer
  "Return the set of entities in the map and layer"
  [m map layer]
  (-> m :silc.core/ave (get entities-in-layer-key) (get (memmlyr map layer))))

(defn should-draw-static-entity?
  [m e]
  (and (pos m e)
       (explored-by-player? m e)
       (att m e :sprite)))

(defn should-draw-mobile-entity?
  [m e]
  (and (should-draw-static-entity? m e)
       (visible-by-player? m e)))

(defn draw-basic-layer!
  "Draw a basic set of entities for the given map and layer
   this simply renders entities with a :sprite and :pos value"
  [game map* layer cw ch]
  (doseq [e (entities-in-layer game map* layer)
          :when (should-draw-static-entity? game e)
          :let [eatts (atts game e)]
          :let [sprite (:sprite eatts)
                [x y] (:pos eatts)]]
    (g/with-color (if (visible-by-player? game e)
                    color/white
                    color/gray)
      (g/draw-point! sprite (* x cw) (* y (- ch))))))

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
  (doseq [e (entities-in-layer game map* :creature)
          :when (should-draw-mobile-entity? game e)]
    (when-let [[x y] (pos game e)]
      (let [x (* x cw)
            y (* y (- ch))]
        (draw-creature-circle! game e x y)
        (g/with-color (if (att game e :hit-time)
                        color/red
                        color/white)
          (draw-creature! game e x y))))))

(defn flag-sprite
  "Returns the flag sprite to use
   for the given move cost (in ap) and current ap
   of the entity"
  [cost ap]
  (cond
    (<= (+ cost 3) ap) :green-flag
    (<= cost ap) :yellow-flag
    :else :red-flag))

(defn draw-ui-path!
  [game]
  (when-let [ui-path (:ui-path game)]
    (let [[cw ch] (cell-size game)
          e (first (selected game))]
      (loop [path ui-path
             last-pos (pos game e)
             last-cost 0]
        (when last-pos
          (let [[pt] path]
            (when-let [[x y] pt]
              (when (explored-by-player-at? game pt)
                (let [cost (move-cost last-pos pt)
                      total-cost (int (+ last-cost cost))
                      spr (flag-sprite total-cost (current-ap game e))]
                  (g/draw-point! spr (* x cw) (- (* y ch)))
                  (recur (rest path)
                         pt
                         total-cost))))))))))

(defn draw-map!
  [game]
  (when (:map game)
    (let [[cw ch] (cell-size game)]
      (draw-basic-layer! game (:map game) :base cw ch)
      (draw-basic-layer! game (:map game) :decor cw ch)
      (draw-basic-layer! game (:map game) :object cw ch)
      (draw-ui-path! game)
      (draw-creature-layer! game (:map game) cw ch))))

(defn draw-world-texts!
  [game]
  (let [[cw ch] (cell-size game)]
    (doseq [world-text (:world-text game)
            :when  (= (:map game) (:map world-text))]
      (g/with-font-color (:color world-text)
        (when-let [[x y] (:pos world-text)]
          (let [y (- (* y (- ch)) -64 (max (:time world-text) 15))]
            (g/draw-text! (:text world-text) (* cw x) y)))))))


(defn draw-world!
  [game]
  (draw-map! game)
  (draw-world-texts! game))


(defn mouse-attack-sprite
  [game]
  (let [target (attackable-at-mouse game)
        e (fselected game)]
    (if (can-attack? game e target)
      :mouse-attack
      :mouse-attack-grey)))

(defn mouse-sprite
  [game]
  (cond
    (attackable-at-mouse game) (mouse-attack-sprite game)
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


(defn draw-ui!
  [game]
  (ui-main/draw! game)
  (draw-screen-positions! game)
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

  (def turn-ender (turn-ender/turn-ender))
  (start turn-ender)
  (stop turn-ender)

  "load a map"
  (def example-map (load-tiled-map "test-resources/test-map.json"))
  (def example-map2 (load-tiled-map "test-resources/test-map2.json"))

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
    (do (send game creates (concat (tiles example-map) (objects example-map)
                                   (tiles example-map2) (objects example-map2)))
        nil)
    "load the tilemap entity itself into the game"
    (let [ent (tiled-map-entity example-map)
          ent2 (tiled-map-entity example-map2)]
      (send game set-atts (:name ent) ent)
      (send game set-atts (:name ent2) ent2)
      nil)
    "set the map to be the example map"
    (do (send game assoc :map :test-map) nil))

  (silent-send! game assoc :map :test-map)
  (silent-send! game assoc :map :test-map2)

  "reset the game to its default"
  (do (restart-agent game default-game)
      nil)
  (silent-send! game (constantly default-game))
  "await the game"
  (await-for 1000 game))
