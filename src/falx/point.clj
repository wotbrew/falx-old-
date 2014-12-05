(ns falx.point
  (:require [clj-tuple :refer [tuple]])
  (:refer-clojure :exclude [+ - / *]))

(def id (tuple 0 0))

(def c+ clojure.core/+)
(def c* clojure.core/*)
(def c- clojure.core/-)

(defn +
  ([[x y] [x2 y2]]
   (tuple (c+ x x2) (c+ y y2))))

(defn north?
  "Is point a (x, y) north of b (x2, y2)"
  [[x y] [x2 y2]]
  (pos? (c- y y2)))

(defn west?
  "Is point a (x, y) west of b (x2, y2)"
  [[x y] [x2 y2]]
  (pos? (c- x x2)))

(defn south?
  "Is point a (x, y) south of b (x2, y2)"
  [[x y] [x2 y2]]
  (neg? (c- y y2)))

(defn east?
  "Is point a (x, y) east of b (x2, y2)"
  [[x y] [x2 y2]]
  (neg? (c- x x2)))

(defn north-east?
  "Is the point a north east of point b"
  [a b]
  (and (north? a b) (east? a b)))

(defn north-west?
  "Is the point a north west of point b"
  [a b]
  (and (north? a b) (west? a b)))

(defn south-west?
  "Is the point a south west of point b"
  [a b]
  (and (south? a b) (west? a b)))

(defn south-east?
  "Is the point a south east of point b"
  [a b]
  (and (south? a b) (east? a b)))

