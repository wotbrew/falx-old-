(ns falx.base
  (:require [silc.core :refer :all]
            [clj-tuple :refer [tuple]]
            [clj-tiny-astar.path :refer [a*]]
            [falx
             [point :as pt]
             [rect :as rect]
             [shapes :as shapes]]
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

(defn mem2
  [f]
  (let [atom (atom {})]
    (fn [a b]
      (or (-> @atom (get a) (get b))
          (let [r (f a b)]
            (swap! atom assoc-in [a b] r)
            r)))))

(def mepmp (mem2 (fn [map pos] {:map map :pos pos})))
(def at-ave-key #{:map :pos})

(defn at
  "Find the entities at the given point (and map)"
  ([game pt]
    (at game (:map game) pt))
  ([game map pt]
    (-> game :silc.core/ave (get at-ave-key) (get (mepmp map pt)))))

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

(defn creature-name
  "Returns the name of the creature"
  [game e]
  (or (att game e :name)
      (str "eid " e)))

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

(defn enemies
  [m]
  (all m :enemy?))

(def selected?
  "Is the entity selected?"
  (att-fn :selected?))

(defn selected
  "Returns all the selected entities"
  [m]
  (all m :selected?))

(defn fselected
  "Returns the first selected entity (or nil)"
  [m]
  (first (selected m)))

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
  (att m e :solid?))

(def solid-at
  "Returns a seq of solid entities at the given point"
  (at-fn solid?))

(def solid-at?
  (comp boolean first solid-at))

(defn solid-at-mouse?
  "Is the cell at the current mouse position solid?"
  [m]
  (solid-at? m (mouse m)))

(defn opaque?
  "Is the entity opaque?"
  [m e]
  (att m e :opaque?))

(def opaque-at
  (at-fn opaque?))

(def opaque-at?
  (comp boolean first opaque-at))

(defn same-map?
  "Are the entities `a` and `b` on the same map?"
  [m a b]
  (= (att m a :map) (att m b :map)))

(defn on-player-map?
  "Is the entity on the same map as at least one player?"
  [m e]
  (some #(same-map? m e %) (players m)))

(defn explored-by?
  [m e observer]
  (and (same-map? m e observer)
       (contains? (att m observer :explored-points) (pos m e))))

(defn explored-by-player?
  [m e]
  (some #(explored-by? m e %) (players m)))

(def explored-by-player-at
  (at-fn explored-by-player?))

(def explored-by-player-at?
  (comp boolean first explored-by-player-at))


(defn visible-by?
  "Is the entity visible by the given `observer` entity?"
  [m e observer]
  (contains? (att m observer :visible-entities) e))

(defn point-visible-by?
  "Is point visible by the given entity?"
  [m e pt]
  (contains? (att m e :visible-points) pt))

(defn visible-by-player?
  "Is the given entity visible by any player?"
  [m e]
  (some #(visible-by? m e %) (players m)))

(def visible-by-player-at
  "Return the visible entities at the given point"
  (at-fn visible-by-player?))

(defn visible-by-player-at?
  "Is the given point visible by any player?"
  [m pt]
  (some #(point-visible-by? m % pt) (players m)))

(defn los*
  [m map apt bpt]
  (take-while #(not (opaque-at? m map %)) (shapes/line apt bpt)))


(defn- los*?
  [m bmap bpt los]
  (if (not (opaque-at? m bmap bpt))
    (= (last los) bpt)
    (when-let [l (last los)]
      (pt/adj? l bpt))))

(defn los-to
  [m a bpt]
  (let [apt (pos m a)]
    (when apt
      (los* m (att m a :map) apt bpt))))

(defn los-to?
  [m a bpt]
  (let [apt (pos m a)]
    (when (and apt bpt)
      (los*? m (att m a :map) bpt (los-to m a bpt)))))

(defn los?
  [m a b]
  (let [amap (att m a :map)
        bmap (att m b :map)
        bpt (pos m b)]
    (when (and bpt (= amap bmap))
      (los-to? m a bpt))))

(def default-visibility-radius
  7)

(defn find-visible-points
  "Returns the visible points for an entity"
  [m e]
  (let [[x y] (pos m e)
        circle (shapes/filled-circle x y default-visibility-radius)]
   (filter #(los-to? m e %) circle)))

(defn find-entities-in
  "Find all the entities in any of the given points"
  [m map points]
  (mapcat #(at m map %) points))



(defn find-visible-entities
  "Returns the entities the current entity can see.
   This does not cache visibility in anyway"
  [m e]
  (->> (find-visible-points m e)
       (mapcat #(at m (att m e :map) %))))


(defn goto
  "LOL - doesn't perform a goto.
   Rather adds the intention to goto the given point."
  [m e pt]
  (update-att m e :thoughts assoc :goto pt))

(defn forget-goto
  "Clears the goto intention from the entity"
  [m e]
  (update-att m e :thoughts dissoc :goto))

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

(defn mode
  "Returns the `mode` of the game.
  i.e :player, :enemy or :real"
  [m]
  (:mode m :player))

(defn mode=
  "Is the current game mode equal to 'mde'
   e.g :player, :enemy or :real"
  [m mde]
  (= (mode m) mde))

(defn map-size
  "Returns the size of the given map as a tuple"
  [m map]
  [(att m map :width) (att m map :height)])

(def default-ap
  "The number of ap to use by default
   if it hasn't already been defined for the entity"
  10)

(defn max-ap
  "Returns the maximum ap (action points)
   that the entity can hold"
  [m e]
  default-ap)

(defn current-ap
  "Returns the current ap (action points)
   that the entity has."
  [m e]
  (or (att m e :ap)
      (max-ap m e)))

(defn any-ap?
  "Does the entity have an ap at all?"
  [m e]
  (pos? (current-ap m e)))

(defn clamp-ap
  "Make sure the action points fit between 0 or (max ap of the entity)"
  [m e ap]
  (max 0 (min ap (max-ap m e))))

(defn set-ap
  "Sets the ap of the entity to the given value"
  [m e ap]
  (set-att m e :ap (clamp-ap m e ap)))

(defn reclamp-ap
  "Make sure our action points fit between 0 or (max)"
  [m e]
  (set-ap m e (current-ap m e)))

(defn update-ap
  "Applies the function to the entities current ap"
  [m e f & args]
  (set-ap m e (apply f (current-ap m e) args)))

(defn refresh-ap
  "'Refreshes' the entities action points
   by setting the entities current ap back to the maximum ap"
  [m e]
  (set-ap m e (max-ap m e)))

(defn refresh-players
  "Refreshes the ap of every player"
  [m]
  (reduce refresh-ap m (players m)))

(defn refresh-enemies
  "Refreshes the ap of every enemy"
  [m]
  (reduce refresh-ap m (enemies m)))

(defn adjacent-points
  "Returns a seq of adjacent points"
  [m e]
  (when-let [pos (pos m e)]
    (pt/adj pos)))


(defn adjacent-to?
  "Is the entity adjacent to the given point?"
  [m e pt]
  (when-let [pos (pos m e)]
    (pt/adj? pos pt)))

(defn adjacent?
  "Are the 2 entities adjacent to each other?"
  [m a b]
  (when-let [pos (pos m b)]
    (and (same-map? m a b)
         (adjacent-to? m a pos))))

(defn all-adjacent-to
  "Return all entities adjacent to the given point"
  [m map pt]
  (mapcat #(at m map %) (pt/adj pt)))

(defn all-adjacent
  "Returns all the entities adjacent to the given entity"
  [m e]
  (when-let [pos (pos m e)]
    (all-adjacent-to m (att m e :map) pos)))

(defn can-act?
  "Can the entity act at all?"
  [m e]
  (and
    (cond
      (player? m e) (mode= m :player)
      (enemy? m e) (mode= m :enemy)
      :else true)
    (pos? (current-ap m e))))

(defn could-move?
  "Could the entity move to the point
   if not for itself due to lack of stamina/ap etc"
  [m e pt]
  (and
    (not (solid-at? m (att m e :map) pt))
    (adjacent-to? m e pt)))

(defn move-cost
  "Returns the cost (in ap) to move from one point to another"
  [a b]
  (int (Math/ceil (pt/precise-dist a b))))

(defn can-move?
  "Can the entity move to the point
   - is it possible?"
  [m e pt]
  (and (could-move? m e pt)
       (can-act? m e)
       (<= (move-cost (pos m e) pt) (current-ap m e))))

(defn move
  "Attempt to move the entity from its current position
   to the one specified"
  [m e pt]
  (if (can-move? m e pt)
    (-> (set-att m e :pos pt)
        (update-ap e - (move-cost (pos m e) pt)))
    m))

(defn should-cancel-all-movement?
  "Should the entity cancel even trying to move"
  [game e]
  (or (dead? game e)
      (not (any-ap? game e))))

(defn should-cancel-move-to-pt?
  "Should cancel attempting to move to the given point
   for example because it cannot make up the ap cost"
  [game e pt]
  (or (should-cancel-all-movement? game e)
      (< (current-ap game e) (move-cost (pos game e) pt))))

(defn path
  "Finds a path for the given entity to the
   given point."
  [game e to]
  (let [pos (pos game e)
        map (att game e :map)
        bounds (map-size game map)
        pred #(or (= pos %) (not (solid-at? game map %)))]
    (when (and pos map bounds)
      (rest (a* bounds pred pos to)))))

(defn path-to-mouse
  "Finds a path for the given entity to the mouse position"
  [game e]
  (when (not (solid-at-mouse? game))
    (path game e (mouse game))))

(defn current-path
  "Returns the current path of the entity"
  [game e]
  (att game e :path []))

(defn next-in-current-path
  "Returns the next pt in the current path"
  [game e]
  (first (current-path game e)))

(defn set-path
  "Sets the entities path to the given value"
  [game e path]
  (set-att game e :path path))

(defn pathing-to-goto?
  "Is the entity currently pathing to its goto?"
  [game e]
  (when-let [goto (:goto (att game e :thoughts))]
    (= goto (last (current-path game e)))))

(defn goto-invalid?
  "Is the entities goto position now invalid?"
  [game e]
  (let [goto (:goto (att game e :thoughts))]
    (cond
      (solid-at? game (att game e :map) goto) :now-solid
      :else nil)))

(defn goto-valid?
  "Is the entities goto position valid?"
  [game e]
  (not (goto-invalid? game e)))

(defn could-move-to-next-in-current-path?
  "Could the entity move to the next point in
   its current path, forgiving lack of AP."
  [game e]
  (when-let [pt (next-in-current-path game e)]
    (could-move? game e pt)))

(defn can-move-to-next-in-current-path?
  [game e]
  (when-let [pt (next-in-current-path game e)]
    (can-move? game e pt)))

(defn current-path-valid?
  "Is the current path valid"
  [game e]
  (and (goto-valid? game e)
       (could-move-to-next-in-current-path? game e)
       (pathing-to-goto? game e)))

(defn step-current-path
  "Attempt to make a single step along the current path"
  [game e]
  (if-let [pt (next-in-current-path game e)]
    (if (can-move? game e pt)
      (-> (move game e pt)
          (update-att e :path rest))
      game)
    game))

(defn hostile-to?
  "Are the entities hostile to one another?"
  [m a b]
  (cond (enemy? m a) (player? m b)
        (player? m a) (enemy? m b)))

(defn adjacent-hostiles
  "Returns a seq of adjacent hostiles"
  [m e]
  (filter #(hostile-to? m e %) (all-adjacent m e)))

(defn could-attack?
  "Can the given entity `a` attack the other one `b`
   - is it possible?

   Does not include an ap check"
  [m a b]
  (and
    (not= a b)
    (creature? m a)
    (creature? m b)
    (adjacent? m a b)))

(defn can-attack?
  "Can the given entity `a` attack the other one `b`
   - is it possible?"
  [m a b]
  (and (could-attack? m a b)
       (can-act? m a)
       (<= 2 (current-ap m a))))

(defn attackable?
  "Is the given entity attackable by the (first) selected entity
   n.b - ignores ap costs"
  [m e]
  (and
    (enemy? m e)
    (could-attack? m (fselected m) e)))

(def attackable-at-mouse
  "Get the attackable entity at the mouse position if possible or nil
   - ignores ap costs"
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
   :map (att m e :map)
   :time 30})

(defn add-world-text
  [m world-text]
  (if (= (:map m) (:map world-text))
    (update m :world-text conj world-text)
    m))

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
  "Have `e` attack `target`"
  [m e target]
  (-> (set-attack-offsets m e target)
      (create-attacked-text target)
      (create-attack-bark e)
      (update-ap e - 2)))

(defn attack
  "Have `e` attack `target` if possible"
  [m e target]
  (if (can-attack? m e target)
    (just-attack m e target)
    m))

(defn attack-at-mouse
  "Attacks the creature at the current mouse position"
  [m]
  (let [e (fselected m)
        target (creature-at-mouse m)]
    (attack m e target)))


(def invert-mode
  {:real :real
   :player :enemy
   :enemy :player})

(defn flip-mode
  [m]
  (assoc m :mode (invert-mode (mode m))))

(defn next-turn
  ""
  [m]
  (-> (case (mode m)
        :enemy (refresh-players m)
        :player (refresh-enemies m)
        m)
      flip-mode))

(def default-cell-size
  "The default cell size used by the game"
  [32 32])

(defn cell-size
  "Return the cell size used by the game"
  [m]
  (:cell-size m default-cell-size))

(def cell-width (comp first cell-size))
(def cell-height (comp second cell-size))

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

(defn move-cam
  "Move the camera to the given point"
  [m pos]
  (assoc m :cam pos))

(defn move-cam-to-entity
  "Points the camera at the given entity"
  [m e]
  (let [[x y] (pos m e)
        [cw ch] (cell-size m)]
    (if (and x y)
      (move-cam m
                (tuple (* x cw)
                       (* y (- ch))))
      m)))

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

(defn key-select
  "Perform the selection and move camera to the entity.
   This should be called when you select a player using the keyboard"
  [m e]
  (-> (perform-select m e)
      (move-cam-to-entity e)))

(defmethod apply-command :select-1
  [m _]
  (key-select m (player m 0)))

(defmethod apply-command :select-2
  [m _]
  (key-select m (player m 1)))

(defmethod apply-command :select-3
  [m _]
  (key-select m (player m 2)))

(defmethod apply-command :select-4
  [m _]
  (key-select m (player m 3)))

(defmethod apply-command :select-5
  [m _]
  (key-select m (player m 4)))

(defmethod apply-command :select-6
  [m _]
  (key-select m (player m 5)))

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

;;creating

(defmulti cook-entity
  "Performs some transformation on the entity to establish some default
    properties when before the entity is created.

    e.g all creatures are solid."
  :type)

(defmethod cook-entity :default
  [ent]
  ent)

(defmethod cook-entity :door
  [ent]
  (let [open? (:open? ent)]
    (assoc ent
      :sprite (if open?
                (:open-sprite ent)
                (:closed-sprite ent))
      :solid? (not open?)
      :opaque? (not open?))))

(defmethod cook-entity :creature
  [ent]
  (assoc ent
    :solid? true))

(defmethod cook-entity :terrain
  [ent]
  (let [terrain (:terrain ent)]
    (assoc ent
      :solid? (= terrain :wall)
      :opaque? (= terrain :wall))))

(defn create-entity
  [m ent]
  (create m (assoc (cook-entity ent)
              :id (id m))))

(defn create-entities
  [m coll]
  (reduce create-entity m coll))

;;screen

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
