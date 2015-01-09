(ns falx.base
  (:require [silc.core :refer :all]
            [clj-tuple :refer [tuple]]
            [falx
             [point :as pt]
             [rect :as rect]]
            [gdx-2d.color :as color]))

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

;;move to silc
(defn update-att
  "Applies the function f (plus any args)
   to the value for the attribute `a` on entity `e`"
  [m e a f & args]
  (set-att m e a (apply f (att m e a) args)))

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
  "Returns whether the given entity is a player"
  [m e]
  (att m e :player?))

(defn players
  "Returns the set of all the players"
  [m]
  (all m :player?))

(defn sorted-players
  [m]
  (sort (players m)))

(defn player
  "Selects the nth player"
  [m n]
  (nth (seq (sorted-players m)) n nil))

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

(defn could-move?
  "Could the entity move to the point
   if not for itself due to lack of stamina/ap etc"
  [m e pt]
  (and
    (not (solid-at? m pt))
    (adjacent-to? m e pt)))

(defn can-move?
  "Can the entity move to the point
   - is it possible?"
  [m e pt]
  (and (could-move? m e pt)))

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

(defn defend-offset
  "Returns the offset point to use for
   defence when a defends against b"
  [m a b]
  (let [pa (pos m a)
        pb (pos m b)]
    (pt/explode (pt/direction pb pa) 4 -4)))

(defn set-attack-offsets
  "Sets the offset positions used for attacking and defending"
  [m e target]
  (-> (set-att m e
               :offset (attack-offset m e target))
      (set-att target
               :offset (defend-offset m target e)
               :hit-time 4)))

(defn entity-world-text
  [m e text color]
  {:text text
   :color color
   :pos (pos m e)
   :time 30})

(defn add-world-text
  [m world-text]
  (update m :world-text conj world-text))

(defn create-attacked-text
  "Creates the attacked text at the target.
   (hovers above the entities head)"
  [m e]
  (add-world-text m (entity-world-text m e "*whack*" color/red)))

(defn create-attack-bark
  "Creates the attack bark at the entity"
  [m e]
  (add-world-text m (entity-world-text m e "haha!" color/white)))

(defn just-attack
  "Has `e` attack `target`"
  [m e target]
  (-> (set-attack-offsets m e target)
      (create-attacked-text target)
      (create-attack-bark e)))

(defn attack
  "Has `e` attack `target` if possible"
  [m e target]
  (if (can-attack? m e target)
    (just-attack m e target)
    m))

(defn attack-at-mouse
  "Attacks the creature at the current mouse position"
  [m]
  (let [e (first (selected m))
        target (creature-at-mouse m)]
    (attack m e target)))

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


(defn handle-primary-in-game
  [m]
  (cond

    (selectable-at-mouse m) (select-at-mouse m)
    (attackable-at-mouse m) (attack-at-mouse m)
    :else (selected-goto-mouse m)))


(defmethod apply-command :select-1
  [m _]
  (perform-select m (player m 0)))

(defmethod apply-command :select-2
  [m _]
  (perform-select m (player m 1)))

(defmethod apply-command :select-3
  [m _]
  (perform-select m (player m 2)))

(defmethod apply-command :select-4
  [m _]
  (perform-select m (player m 3)))

(defmethod apply-command :select-5
  [m _]
  (perform-select m (player m 4)))

(defmethod apply-command :select-6
  [m _]
  (perform-select m (player m 5)))

(def anim-speed 100)

(defn tick-offset
  [offset delta]
  (pt/+ offset (pt/explode (pt/direction offset pt/id) delta delta)))

(defn animate-offset
  "Animate the offset on the given entity
   this changes depending on the animation speed
   this is the offset added after certain actions"
  [m e]
  (let [[x y] (att m e :offset)]
    (if (and (<= -1 x 1) (<= -1 y 1))
      (delete-att m e :offset)
      (update-att m e :offset tick-offset (* (:delta m 0) (/ anim-speed 4))))))

(defn animate-offsets
  "Animates the offsets of any entity that has one"
  [m]
  (reduce animate-offset m (having m :offset)))

(defn animate-hit-frame
  "Animates the hit frame of the given entity
   this makes the entity flash red for a time after being attacked"
  [m e]
  (let [time (att m e :hit-time)]
    (if (<= time 0)
      (delete-att m e :hit-time)
      (update-att m e :hit-time -  (* (:delta m 0) (/ anim-speed 4))))))

(defn animate-hit-frames
  "Animates the hit frames of any entity that has them"
  [m]
  (reduce animate-hit-frame m (having m :hit-time)))


(defn animate-world-text
  [m world-text]
  (when (pos? (:time world-text))
    (update world-text :time - (* (:delta m 0) (/ anim-speed 4)))))

(defn animate-word-texts
  [m]
  (update m :world-text #(keep (partial animate-world-text m) %)))

(defn simulate
  "Performs any necessary ambient simulation steps
   (like animation)"
  [m]
  (-> m
      animate-offsets
      animate-hit-frames
      animate-word-texts))

;;screen

(def default-cell-size
  "The default cell size used by the game"
  [32 32])

(defn cell-size
  "Return the cell size used by the game"
  [m]
  (:cell-size m default-cell-size))

(def cell-width (comp first cell-size))
(def cell-height (comp second cell-size))

(defn mouse-cell
  "Return the pt x,y in the world
  that the mouse is currently over."
  [m]
  (let [[x y] (or (:mouse-world m) pt/id)
        [w h] (cell-size m)]
    (tuple (idiv x w)
           (idiv (- y h) (- h)))))

(def default-screen [1024 768])

(defn screen
  [m]
  (:screen m default-screen))

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

(defn player-buffer
  "Returns the buffer relevant to the screen origin
   for the given player (by n)"
  [game n]
  (let [[x y _ h] (if (< n 3) (left-buffer game) (right-buffer game))]
    (tuple x (+ y h -160 (* (mod n 3) -192)) 128 160)))

(defn mouse-in?
  "Is the mouse currently in the given rect"
  ([game [x y w h]]
    (mouse-in? game x y w h))
  ([game x y w h]
    (let [[mx my] (:mouse-screen game pt/id)
          [_ sh] (screen game)
          my (- sh my)]
      (rect/pt-in? x y w h mx my))))

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

(defn handle-primary-ui
  "Handles clicks in the ui area"
  [m]
  (-> m
      handle-primary-player-buffers))

(defmethod apply-command :primary
  [m _]
  (if (mouse-in-game? m)
    (handle-primary-in-game m)
    (handle-primary-ui m)))

(defprotocol ILifecycle
  (start [this])
  (stop [this]))
