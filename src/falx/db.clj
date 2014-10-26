(ns falx.db
  "A mini datomic like memory store with indexing, integrity checking etc."
  (:require [falx.util :refer :all]))

(def default-id (bigdec 0))

(defn next-id
  "Return the next-id that will be used if you create a new entity"
  [m]
  (::id m default-id))

(defn- inc-id
  "Increments the id and returns the db"
  [m]
  (update m ::id (fnil inc default-id)))

(defn- rem-eav
  [m id attr]
  (dissoc-in m [::eav id attr]))

(defn- set-eav
  [m id attr value]
  (assoc-in m [::eav id attr] value))

(def set-disj (fnil disj #{}))

(defn- rem-ae
  [m id attr]
  (let [set (-> m ::ae (get attr) (disj id))]
    (if (empty? set)
      (dissoc-in m [::ae attr])
      (assoc-in m [::ae attr] set))))

(defn- set-ae
  [m id attr]
  (update-in m [::aev attr] set-conj id))

(defn rem-ave
  [m id attr value]
  (let [set (-> m ::ave (get attr) (get value) (disj id))]
    (if (empty? set)
      (dissoc-in m [::ave attr value])
      (assoc-in m [::ave attr value] set))))

(defn- set-ave
  [m id attr value]
  (update-in m [::ave attr value] set-conj id))

(defn- index
  [m id attr value]
  (cond->
   (set-eav m id attr value)
   (get (::ae? m) attr) (set-ae id attr)
   (get (::ave? m) attr) (set-ave id attr value)))

(defn- unindex
  [m id attr value]
  (cond->
   (rem-eav m id attr)
   (get (::ae? m) attr) (rem-ae id attr)
   (get (::ave? m) attr) (rem-ave id attr value)))

(defn valid?
  "Verify the attribute via any integrity checks setup for the db"
  [m id attr value]
  (let [rules (::integrity m)]
    (or (empty? rules)
        (some #(% m id attr value) rules))))

(defn rem-attr
  "Removes the attribute from the entity."
  [m id attr]
  (let [val (-> m ::eav (get id) (get attr))]
    (unindex m id attr val)))

(defn set-attr
  "Set the attribute to the given value. Verifies validity via integrity constraints."
  [m id attr value]
  (if (valid? m id attr value)
    (->
     (rem-attr m id attr)
     (index id attr value))
    m))

(defn set-attrs
  "Set attributes given by the map `attrs`"
  [m id attrs]
  (reduce (fn [m [k v]] (set-attr m id k v)) m attrs))

(defn attr
  "Returns the value for the attribute on entity given by `id`.
   Defaulting to `else` the attribute does not exist."
  ([m id attr else]
     (-> m ::eav (get id) (get attr else)))
  ([m id attr]
     (falx.db/attr m id attr nil)))

(defn attrs
  "Returns all the attributes of the given entity as a map"
  [m id]
  (-> m ::eav (get id)))

(defn create
  "Creates the entity using the initial map of attributes `attrs`.
   Returns a pair of [id, new-db]"
  [m attrs]
  (let [id (next-id m)
        m (set-attrs (inc-id m) id attrs)]
    [id m]))

(defn creates
  "Creates many entities with the attributes given by maps in `coll`"
  [m coll]
  (reduce (comp second create) m coll))

(defn delete
  "Removes an entity entirely"
  [m id]
  (reduce #(rem-attr %1 id (first %2)) m (attrs m id)))

(defn entities
  "Returns a seq of all the entity ids"
  [m]
  (keys (::eav m)))

(defn by-val
  "Return a set of entities that have the given value. For attr. If the attribute isn't indexed
   in the `ave` index, a linear scan takes place."
  [m attr value]
  (or 
   (-> m ::ave (get attr) (get value))
   (set (filter #(= (falx.db/attr m % attr) value) (entities m)))))

(defn with-attr
  "Return a set of entities that have the given attribute. If the attribute isn't indexed
   in the `ae` index, a linear scan takes place."
  [m attr]
  (or
   (-> m ::ae (get attr))
   (set (filter #(-> m ::eav (get %) (contains? attr)) (entities m)))))

(defn update-attr
  "Apply a function to a given entity attribute.
   The current value of the attribute will be supplied as the first arg to the fn."
  [m id attr f & args]
  (let [current (falx.db/attr m id attr)]
    (set-attr m id attr (apply f current args))))

(defn update-with-attr
  "Apply a function to each value of a given attribute (all entities)
   The current value of the attribute will be supplied as the first arg to the fn/"
  [m attr f & args]
  (let [having (with-attr m attr)]
    (reduce #(apply update-attr %1 %2 attr f args) m having)))
