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
            [clojure.string :as str]
            [falx.state :as state]))


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

(defn bottom-left-buffer
  [game]
  (tuple 0 0 (* 4 32) (* 6 32)))

(defn bottom-right-buffer
  "Returns the bottom right buffer rect relative to the current screen origin"
  [game]
  (tuple (- (width game) (* 4 32)) 0 (* 4 32) (* 6 32)))

(defn log-buffer
  [game]
  (tuple (- (width game) (* 18 32)) 0 (* 14 32) (* 6 32)))

(defn player-buffer
  "Returns the buffer relevant to the screen origin
   for the given player (by n)"
  [game n]
  (let [[x y _ h] (if (< n 3) (left-buffer game) (right-buffer game))]
    (tuple x (+ y h -160 (* (mod n 3) -192)) 128 160)))

(defn player-buffers
  "Returns a seq of the player buffers"
  [game]
  (map #(player-buffer game %) (range 6)))


(defn initiative-buffer
  "Returns the initiative buffer for the 'nth' entity"
  [game n]
  (let [[x y] (game-buffer game)]
    (tuple (+ x (* (inc n) 32)) y 32 32)))

(defn mouse-in-game?
  "Is the mouse currently in the game buffer?"
  [game]
  (mouse-in? game (game-buffer game)))


(defn player-n-having-mouse
  "Returns the player (index) for that buffer which
   has the mouses focus. Returns nil if the mouse is currently not in any
   player buffer"
  [game]
  (loop [n 0]
    (when (< n 6)
      (let [buffer (player-buffer game n)]
        (if (mouse-in? game buffer)
          n
          (recur (inc n)))))))

;;mouse clicks
(defn handle-primary-player-buffers
  "Handles clicks in the player buffers area"
  [m]
  (if-let [n (player-n-having-mouse m)]
    (perform-select m (player m n))
    m))

(defn bottom-right-buffer-button-n
  "Returns the nth bottom right button buffer"
  [m n]
  (let [[x y w] (bottom-right-buffer m)]
    (tuple x (+ y 160 (* n -32)) w 32)))

(defn end-turn-button-buffer
  "Returns the end turn button buffer"
  [m]
  (bottom-right-buffer-button-n m 0))

(defn enemy-turn-buffer
  "Returns the buffer used to display the enemies turn in flight"
  [m]
  (bottom-right-buffer-button-n m 0))

(defn handle-primary-bottom-right-buttons
  "Handles clicks in the bottom right buffer area"
  [m]
  (cond
    (and (mode= m :player) (mouse-in? m (end-turn-button-buffer m))) (next-turn m)
    :else m))

(defn handle-primary
  "Handles clicks in the ui area"
  [m]
  (-> m
      handle-primary-player-buffers
      handle-primary-bottom-right-buttons))

(defn handle-secondary
  "Handles right clicks in ui area"
  [m]
  m)

;;rendering

;;debug

(def pprint-str #(with-out-str (clojure.pprint/pprint %)))

(def mem-pprint-str
  "A memoized version of pprint that returns a string"
  (mem/lru pprint-str :lru/threshold 10))

(def mem-filter-keys
  "A memoized version of filter keys"
  (mem/lru filter-keys :lru/threshold 10))

(def do-not-show-keys #{:visible-entities :visible-points :explored-points})

(defn debug-entity-attributes
  "Returns a debug string for the given entities attributes"
  [game e]
  (str
    "entity: " e
    "\n"
    (mem-pprint-str (mem-filter-keys (atts game e) #(and (keyword? %)
                                                         (not (do-not-show-keys %)))))))

(defn debug-str
  "Returns a debug string for the given game state"
  [game]
  (str/join
    "\n"
    (concat [(select-keys game [:fps :screen :map])
             (select-keys game [:mouse-screen :mouse-world :mouse-cell])
             (str "commands: " (:commands game))
             (str "in-game? " (mouse-in-game? game))
             (str "mode: " (mode game))
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
  (let [[x y w h] (log-buffer game)]
    (draw-blanks! (- x 32) y 32 h))
  (let [[x y w h] (bottom-left-buffer game)]
    (draw-blanks! w y 32 h))
  (let [[x y w h] (bottom-right-buffer game)]
    (draw-blanks! (- x 32) y 32 h)))

(defn draw-turn-button!
  [game [x y w h]]
  (draw-button! game :turn "Next Turn" x y w h))

(defn draw-enemy-turn!
  [m [x y w h]]
  (draw-box! color/gray x y w h)
  (g/draw-quad! :turn x y 32 32)
  (g/with-font-color color/red
    (g/draw-text! "Enemy Turn" (+ x 32) (+ y 23))))

(defn draw-bottom-right-buttons!
  [game]
  (when (mode= game :player)
    (draw-turn-button! game (end-turn-button-buffer game)))
  (when (mode= game :enemy)
    (draw-enemy-turn! game (enemy-turn-buffer game))))

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


(defn draw!
  [game]
  (draw-buffers! game)
  (draw-bottom-right-buttons! game)
  (draw-players! game)
  (draw-debug! game))
