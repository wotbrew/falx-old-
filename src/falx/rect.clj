(ns falx.rect)

(defn pt-in?
  ([x y width height x2 y2]
   (and (<= x x2 (+ x width))
        (<= y y2 (+ y height))))
  ([x y width height [x2 y2]]
   (pt-in? x y width height x2 y2)))