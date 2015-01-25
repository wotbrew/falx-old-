(ns falx.shapes
  (:require [clj-tuple :refer [tuple]]
            [falx.point :as pt]
            [falx.rect :as rect]))

(defn line*
  [x1 y1 x2 y2 dx dy sx sy err]
  (lazy-seq
    (if (and (= x1 x2)
             (= y1 y2))
      (cons (tuple x2 y2) nil)
      (cons (tuple x1 y1)
            (let [e2 (* 2 err)
                  err (if (> e2 (- dy))
                        (- err dy)
                        err)
                  err (if (< e2 dx)
                        (+ err dx)
                        err)
                  x1 (if (> e2 (- dy))
                       (+ x1 sx)
                       x1)
                  y1 (if (< e2 dx)
                       (+ y1 sy)
                       y1)]
              (line*
                x1 y1 x2 y2 dx dy sx sy err))))))

(defn line
  ([[x1 y1] [x2 y2]]
    (line x1 y1 x2 y2))
  ([x1 y1 x2 y2]
    (let [dx (Math/abs ^int (- x2 x1))
          dy (Math/abs ^int (- y2 y1))
          sx (if (< x1 x2) 1 -1)
          sy (if (< y1 y2) 1 -1)
          err (- dx dy)]
      (line* x1 y1 x2 y2 dx dy sx sy err))))

(defn circle
  [^double x ^double y ^double radius]
  (loop [res (list
               (tuple x (+ y radius))
               (tuple x (- y radius))
               (tuple (+ x radius) y)
               (tuple (- x radius) y))
         f (- 1 radius)
         ddf-x 1
         ddf-y (* -2 radius)
         xx 0
         yy radius]
    (if (< xx yy)
      (let [yy (if (>= f 0) (dec yy) yy)
            ddf-y (if (>= f 0) (+ ddf-y 2) ddf-y)
            f (if (>= f 0) (+ f ddf-y) f)
            xx (inc xx)
            ddf-x (+ ddf-x 2)
            f (+ f ddf-x)]
        (recur
          (conj res
                (tuple (+ x xx) (+ y yy))
                (tuple (- x xx) (+ y yy))
                (tuple (+ x xx) (- y yy))
                (tuple (- x xx) (- y yy))

                (tuple (+ x yy) (+ y xx))
                (tuple (- x yy) (+ y xx))
                (tuple (+ x yy) (- y xx))
                (tuple (- x yy) (- y xx)))
          f
          ddf-x
          ddf-y
          xx
          yy))
      (set res))))

(defn filled-circle
  [x y radius]
  (for [xx (range (- x radius) (+ x radius 1))
        yy (range (- y radius) (+ y radius 1))
        :when (<= (pt/manhattan-dist x y xx yy) radius)]
    (tuple xx yy)))


(defn square
  [x y width]
  (rect/pts (- x (int (/ width 2))) (- y (int (/ width 2))) width width))