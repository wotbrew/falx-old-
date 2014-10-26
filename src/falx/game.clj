(ns falx.game
  (:require [falx.db :as db]
            [falx.util :refer :all]
            [clj-tuple :refer [tuple]]
            [clojure.string :as str]))

(defn at
  [m pos]
  (db/by-val m :pos pos))

(defn fat
  [m pred pos]
  (first (filter #(pred m %) (at m pos))))

(defmacro deflag
  ([attr
    question question-doc
    all all-doc]
     `(deflag ~attr
        ~question ~question-doc
        ~all ~all-doc
        nil nil))
  ([attr
    question question-doc
    all all-doc
    at at-doc]
     (let [m (symbol "m")
           id (symbol "id")
           pos (symbol "pos")]
       `(do
          (defn ~question
            ~question-doc
            [~m ~id]
            (db/attr ~m ~id ~attr))
          (defn ~all
            ~all-doc
            [~m]
            (db/by-val ~m ~attr true))
          ~(when at
            `(defn ~at
              ~at-doc
              [~m ~pos]
              (fat ~m ~question ~pos)))))))

(deflag :creature?
  creature? "Is the entity a creature"
  creatures "Return all the creatures"
  creature-at "Return the creature at position")

(deflag :player?
  player? "Is the entity a player"
  players "Return all the players"
  player-at "Return the player at position")

(defn player-at-mouse
  "Return the player at mouse position"
  [m]
  (player-at m (:mouse-pos m)))

(defn nth-player
  "Find the nth player (or nil)"
  [m n]
  (nth (seq (players m)) n nil))

(defn selectable?
  "Is the entity selectable?"
  [m id]
  (player? m id))

(defn selectable-at-mouse
  "Return the selectable entity at the mouse position"
  [m]
  (fat m selectable? (:mouse-pos m)))

(deflag :selected?
  selected? "Is the entity selected"
  selected "Return all the selected entities")

(defn unselect
  [m id]
  (db/rem-attr m :selected?))

(defn unselect-all
  [m]
  (reduce unselect m (selected m)))

(defn select
  [m id]
  (if (selectable? m id)
    (db/set-attr m id :selected? true)
    m))

(defn select-only
  [m id]
  (select (unselect-all m) id))

(defn invert-select
  [m id]
  (if (selected? m id)
    (unselect m id)
    (select m id)))

(defn mouse-select
  [m]
  (if-let [id (selectable-at-mouse m)]
    (invert-select m id)
    m))

(defn mouse-select-only
  [m]
  (if-let [id (selectable-at-mouse m)]
    (select-only m id)
    m))

(deflag :solid?
  solid? "Is the entity solid?"
  solids "Return all the solid entities"
  solid-at "Return the first solid entity at the position")

(def solid-at? (comp boolean solid-at))

(defn put
  [m id pos]
  (if (and (solid? m id) (solid-at? m pos))
    m
    (db/set-attr m id :pos pos)))



;;screen

(def default-screen [1024 768])

(defn screen
  "Return the screen width and height as a tuple."
  [m]
  (:screen m default-screen))

(defn top-left
  "Return the top-left screen co-ordinates"
  [m]
  (let [[w h] (screen m)]
    (tuple (/ w -2) (/ h 2))))

(defn mid-left
  "Return the mid-left screen co-ordinates"
  [m]
  (let [[w h] (screen m)]
    (tuple (/ w -2) 0)))

(defn bottom-left
  "Return the bottom-left screen co-ordinates"
  [m]
  (let [[w h] (screen m)]
    (tuple (/ w -2) (/ h -2))))


(comment
  "top left"
  (top-left {:screen [1024 768]})
  (top-left nil)
  "mid left"
  (mid-left {:screen [1024 768]})
  "bottom-left"
  (bottom-left {:screen [1024 768]}))

;;ui

(def default-cell-size
  "The default size of a cell in the game, e.g [32,32]"
  (tuple 32 32))

(defn cell-size
  "Return the cell size used by the game"
  [m]
  (:cell-size m default-cell-size))

(def default-mouse-cell
  (tuple 0 0))

(defn mouse-cell
  "Return the pt x,y in the world 
  that the mouse is currently over."
  [m]
  (let [[x y] (:mouse-world m default-mouse-cell)
        [w h] (cell-size m)]
    (tuple (int (/ x w))
           (int (/ (- y h) (- h))))))

(defn mouse-pos
  "Return the pos triple world,x,y that the mouse is currently over"
  [m]
  (let [world (:current-world m)
        [x y] (:mouse-cell m)]
    (tuple world x y)))

(defn mderive
  "Take a map a key and a fn. Apply the fn to the map and assoc the result with the key into the map"
  [m key f]
  (assoc m key (f m)))

(defn ui-frame
  "Perform a set of ui updates
  that should be applied eagerly every frame"
  [m]
  (-> (mderive m :mouse-cell mouse-cell)
      (mderive :mouse-pos mouse-pos)))

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
  (if (command-hit? m :mod)
    cam-fast-speed
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

(defn select-mod
  [m id]
  (if (command-hit? m :mod)
    (invert-select m id)
    (select-only m id)))

(defmethod apply-command :primary
  [m _]
  (cond
   (player-at-mouse m) (select-mod m (player-at-mouse m))
   :else m))

(dotimes [x 6]
  (defmethod apply-command (keyword (str "select-" (inc x)))
    [m _]
    (select-mod m (nth-player m x))))

;;debug

(defn mouse-debug
  "Return some debug information about the mouse position"
  [m]
  {:cell (mouse-cell m)
   :world (:mouse-world m)
   :screen (:mouse-screen m)
   :pos (:mouse-pos m)})

(defn entity-debug
  [m]
  {:at (at m (:mouse-pos m))
   :cr (creature-at m (:mouse-pos m))
   :sel (selected m)
   :ply (players m)})

(defn general-debug
  "Return some generic debug information"
  [m]
  (unlines [(mouse-debug m)
            (entity-debug m)
            (select-keys m [:fps :current-world :commands])]))

(defn debug
  "Return the current debug information for the game"
  [m]
  (general-debug m))

;; default setup

(def default-key-bindings
  "A default set of keybindings to commands that can be used for the game"
  [[:pressed :w, :cam-up]
   [:pressed :a, :cam-left]
   [:pressed :d, :cam-right]
   [:pressed :s, :cam-down]
   [:pressed :lshift, :mod]
   [:hit :left, :primary]
   [:hit :f1, :select-1]
   [:hit :f2, :select-2]
   [:hit :f3, :select-3]
   [:hit :f4, :select-4]
   [:hit :f5, :select-5]
   [:hit :f6, :select-6]])






