(ns falx.util
  (:require [clj-tuple :refer [tuple]])
  (:import (clojure.lang MapEntry)))

(defn update
  [m k f & args]
  (assoc m k (apply f (get m k) args)))


(defn ind
  [x y w]
  (+ x (* y w)))

(defn rind
  [i w]
  [(int (mod i w)) (int (/ i w))])

(defn div-rect
  [[x y w h] nx ny]
  [(int (/ x nx))
   (int (/ y ny))
   (int (/ w nx))
   (int (/ h ny))])

(defn map-vals
  [m f]
  (->>
    (for [[k v] m]
      (MapEntry. k (f v)))
    (into {})))

(defn juxt-map
  [m]
  (fn [v]
    (map-vals m #(% v))))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(def set-conj (fnil conj #{}))
(def vec-conj (fnil conj []))

(defn pos->pt
  [[_ x y]]
  (tuple x y))

(defn pt->pos
  [[x y] world]
  (tuple world x y))

(defn shift
  ([pt [x y]]
   (shift pt x y))
  ([pt x y]
   (if pt
     (-> pt
         (update 0 + x)
         (update 1 + y))
     (tuple x y))))
