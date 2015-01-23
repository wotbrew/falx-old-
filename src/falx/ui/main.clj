(ns falx.ui.main
  (:require [falx.base :refer :all]
            [falx.rect :as rect]
            [falx.point :as pt]
            [falx.ui.shared :refer :all]
            [silc.core :refer :all]
            [clj-tuple :refer [tuple]]
            [gdx-2d.core :as g]
            [gdx-2d.color :as color]
            [clojure.core.memoize :as mem]
            [clojure.string :as str]))


(defn game-buffer
  "Returns the game buffer rect relative to the current screen origin"
  [game]
  (tuple (* 5 32) (* 7 32)
         (- (width game) (* 10 32))
         (- (height game) (* 7 32))))

(defn left-buffer
  "Returns the left buffer rect relative to the current screen origin."
  [game]
  (tuple 0 0 (* 4 32) (height game)))

(defn bottom-buffer
  "Returns the bottom buffer rect relative to the current screen origin."
  [game]
  (tuple 0 0 (width game) (* 6 32)))

(defn right-buffer
  "Returns the right buffer rect relative to the current screen origin."
  [game]
  (tuple (- (width game) (* 4 32)) 0 (* 4 32) (height game)))

(defn bottom-right-buffer
  "Returns the bottom right buffer rect relative to the current screen origin"
  [game]
  (tuple (- (width game) (* 4 32)) 0 (* 4 32) (* 6 32)))

(defn player-buffer
  "Returns the buffer relevant to the screen origin
   for the given player (by n)"
  [game n]
  (let [[x y _ h] (if (< n 3) (left-buffer game) (right-buffer game))]
    (tuple x (+ y h -160 (* (mod n 3) -192)) 128 160)))

(defn initiative-buffer
  "Returns the initiative buffer for the 'nth' entity"
  [game n]
  (let [[x y] (game-buffer game)]
    (tuple (+ x (* (inc n) 32)) y 32 32)))

(defn mouse-in-game?
  "Is the mouse currently in the game buffer?"
  [game]
  (mouse-in? game (game-buffer game)))

;;mouse clicks
(defn handle-primary-player-buffers
  "Handles clicks in the player buffers area"
  [m]
  (loop [m m
         n 0]
    (if (< n 6)
      (let [buffer (player-buffer m n)]
        (if (mouse-in? m buffer)
          (perform-select m (player m n))
          (recur m (inc n))))
      m)))

(defn handle-primary-bottom-right-buttons
  [m]
  (let [[x y w h :as buffer] (bottom-right-buffer m)]
    (if (mouse-in? m buffer)
      (cond
        (mouse-in? m x (+ y 160) w h) (do
                                        (println "end turn hit")
                                        m)
        :else m)
      m)))

(defn handle-primary
  "Handles clicks in the ui area"
  [m]
  (-> m
      handle-primary-player-buffers
      handle-primary-bottom-right-buttons))

;;rendering

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

(defn draw-debug!
  "Draws some useful debug information to the screen"
  [game]
  (let [[x] (game-buffer game)
        [_ sh] (screen game)]
    (g/draw-text! (debug-str game) x sh)))



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
    (draw-blanks! 0 h w 32 32 32))
  (let [[x y w h] (bottom-right-buffer game)]
    (draw-blanks! (- x 32) y 32 h)))


(defn draw-turn-button!
  [game x y w h]
  (draw-button! game :turn "Next Turn" x y w 32))

(defn draw-bottom-right-buttons!
  [game]
  (let [[x y w h] (bottom-right-buffer game)]
    (draw-turn-button! game x (+ y 160) w h)))

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
      (g/draw-quad! spr (+ x 32) (+ y h -96) 64 64)
      (g/draw-text! (str (current-ap game player) "AP") (+ x 8) (+ y h -128)))))

(defn draw-players!
  [game]
  (dotimes [n 6]
    (when-let [player (player game n)]
      (draw-player! game player (player-buffer game n)))))


(defn draw-initiative!
  [game e x y w h]
  (draw-filled-box! x y w h)
  (g/draw-quad! (att game e :sprite :blank) x y w h)
  (g/draw-text! "0" x (+ y h))
  (when (mouse-in? game x y w h)
    (draw-hover-text! (creature-name game e) x (+ y h))))

(defn draw-combat-order!
  [game]
  (let [cr (combatants game)]
    (loop [cr cr
           i 0]
      (let [e (first cr)
            [x y w h] (initiative-buffer game i)]
        (when e
          (draw-initiative! game e x y w h)
          (recur (rest cr) (inc i)))))))

(defn draw!
  [game]
  (draw-buffers! game)
  (draw-bottom-right-buttons! game)
  (draw-players! game)
  (draw-combat-order! game)
  (draw-debug! game))