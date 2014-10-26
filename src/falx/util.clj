(ns falx.util
  "Contains utility functions useful in any context"
  (:require [clj-tuple :refer [tuple]]
            [clojure.string :as str])
  (:import (clojure.lang MapEntry)))

(defn unlines
  [coll]
  (str/join "\n" coll))

(defn update
  "Like update-in but applies a fn at location given by key."
  [m k f & args]
  (assoc m k (apply f (get m k) args)))

(defn ind
  "Index map - take co-ords and a width and translate into a 
  single numeric value."
  [x y width]
  (+ x (* y width)))

(defn rind
  "Reverse index map given an index `i` and a width."
  [i width]
  [(int (mod i width)) (int (/ i width))])

(defn div-rect
  "Divides the rect coercing resulting numeric values to integers.
  x, w is divided by `nx` 
  y, h is divided by `ny`.
  Returns a new rect."
  [[x y w h] nx ny]
  [(int (/ x nx))
   (int (/ y ny))
   (int (/ w nx))
   (int (/ h ny))])

(defn map-vals
  "Take a map `m` and a function `f`, applies the function to each value in the map.
  Returns a new map"
  [m f]
  (->>
    (for [[k v] m]
      (MapEntry. k (f v)))
    (into {})))

(defn juxt-map
  "Take a map `m` of keys to functions. Returns a function 
  that when applied will apply each function to the argument
  associating the result with the keyword in the original map.
  e.g ((juxt-map {:foo inc :bar dec}) 0) => {:foo 1, :bar -1}"
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

(def set-conj
  "Like conj, but if the first arg is nil, then simply return a singleton set"
  (fnil conj #{}))

(def vec-conj
  "Like conj, but if the first arg is nil, then simply return a singleton vector"
  (fnil conj []))

(defn pos->pt
  "Take a position (world, x, y) triple and derives a point from it"
  [[_ x y]]
  (tuple x y))

(defn pt->pos
  "Take a point and a world, return a new position triple"
  [[x y] world]
  (tuple world x y))

(defn shift
  "Take a point and 2 numeric values - x, y or an x, y pair.
  returns a new point shifted by x and y."
  ([pt [x y]]
   (shift pt x y))
  ([pt x y]
   (if pt
     (-> pt
         (update 0 + x)
         (update 1 + y))
     (tuple x y))))

(defn north?
  "Is point a (x, y) north of b (x2, y2)"
  [[x y] [x2 y2]]
  (pos? (- y y2)))

(defn west?
  "Is point a (x, y) west of b (x2, y2)"
  [[x y] [x2 y2]]
  (pos? (- x x2)))

(defn south?
  "Is point a (x, y) south of b (x2, y2)"
  [[x y] [x2 y2]]
  (neg? (- y y2)))

(defn east?
  "Is point a (x, y) east of b (x2, y2)"
  [[x y] [x2 y2]]
  (neg? (- x x2)))

(def map->size
  "Take a map `m` that contains at least the keys, :width and :height. 
  returns a tuple of the :width and :height."
  (juxt :width :height))
