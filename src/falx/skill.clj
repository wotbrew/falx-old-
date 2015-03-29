(ns falx.skill)

(def mechanisms
  {:name :mechanisms
   :sprite :mechanisms
   :map-skill? true})

(def stealth
  {:name :stealth
   :sprite :stealth
   :map-skill? true})

(def perception
  {:name :perception
   :sprite :perception
   :map-skill? true})

(def all
  [mechanisms
   stealth
   perception])

(def by-name
  (->> all
       (reduce #(assoc %1 (:name %2) %2) {})))

(def map-usable
  (filterv :map-skill? all))

(def map-usable-indexed
  (into [] (map-indexed vector map-usable)))