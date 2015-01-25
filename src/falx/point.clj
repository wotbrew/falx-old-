(ns falx.point
  (:require [clj-tuple :refer [tuple]])
  (:refer-clojure :exclude [+ - *])
  (:import (java.util Deque Set HashSet LinkedList)))

(def id (tuple 0 0))

(def c+ clojure.core/+)
(def c* clojure.core/*)
(def c- clojure.core/-)

(defn +
  ([[x y] [x2 y2]]
   (tuple (c+ x x2) (c+ y y2))))

(defn -
  ([[x y] [x2 y2]]
   (tuple (c- x x2) (c- y y2))))

(defn *
  ([[x y] [x2 y2]]
   (tuple (c* x x2) (c* y y2))))

(defn explode
  [[x y] xn yn]
  (tuple (c* x xn) (c* y yn)))

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

(defn manhattan-dist
  ([[x0 y0] [x1 y1]]
   (manhattan-dist x0 y0 x1 y1))
  ([x0 y0 x1 y1]
   (c+ (Math/abs ^int (c- x1 x0)) (Math/abs ^int (c- y1 y0)))))

(defn precise-dist
  ([[x y] [x2 y2]]
    (precise-dist x y x2 y2))
  ([^double x0 ^double y0 ^double x1 ^double y1]
    (let [dx (c- x1 x0)
          dy (c- y1 y0)
          dx (c* dx dx)
          dy (c* dy dy)]
      (Math/sqrt (c+ dx dy)))))


(defn adj?
  ([[x y] [x2 y2]]
   (adj? x y x2 y2))
  ([x1 y1 x2 y2]
   (and (<= (Math/abs ^int (c- x1 x2)) 1)
        (<= (Math/abs ^int (c- y1 y2)) 1))))

(defn magnitude
  ([[x y]]
   (magnitude x y))
  ([x y]
   (Math/sqrt
     (c+ (c* x x)
         (c* y y)))))

(defn normalize
  ([[x y]]
   (normalize x y))
  ([x y]
   (let [m (magnitude x y)]
     (tuple (/ x m)
            (/ y m)))))

(defn direction
  [a b]
  (normalize (- b a)))

(defn intify
  [[x y]]
  (tuple (int x) (int y)))


(def dir->pt
  {:n (tuple 0 -1)
   :ne (tuple 1 -1)
   :e (tuple 1 0)
   :se (tuple 1 1)
   :s (tuple 0 1)
   :sw (tuple -1 1)
   :w (tuple -1 0)
   :nw (tuple -1 -1)})

(def dirs
  (vals dir->pt))

(def cardinal?
  #{:n :e :s :w})

(def cardinal-dirs
  (for [[d vec] dir->pt
        :when (cardinal? d)]
    vec))

(defn adj
  ([pt]
    (map #(+ % pt) dirs)))

(defn cardinal-adj
  ([pt]
    (map #(+ % pt) cardinal-dirs)))

;;todo efficient functional algorithm here?
(defn- flood*
  [^Deque q ^Set hs pred]
  (lazy-seq
    (when (< 0 (.size q))
      (let [n (.poll q)]
        (if (pred n)
          (do
            (doseq [a (cardinal-adj n)
                    :when (.add hs a)]
              (.addFirst q a))
            (cons n (flood* q hs pred)))
          (flood* q hs pred))))))

(defn flood
  [p pred]
  (let [hs (HashSet.)
        q (LinkedList.)]
    (.add hs p)
    (.add q p)
    (flood* q hs pred)))


