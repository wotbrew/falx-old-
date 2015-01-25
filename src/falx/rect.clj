(ns falx.rect
  (:require [clj-tuple :refer [tuple]]))

(defn pt-in?
  ([x y width height x2 y2]
   (and (<= x x2 (+ x width))
        (<= y y2 (+ y height))))
  ([x y width height [x2 y2]]
   (pt-in? x y width height x2 y2)))

(defn pts
  ([x y w h]
    (for [xn (range 0 w)
          yn (range 0 h)]
      (tuple (+ x xn) (+ y yn))))
  ([[x y w h]]
    (pts x y w h)))
