(ns falx.ui.shared
  (:require [gdx-2d.color :as color]
            [gdx-2d.core :as g]
            [falx.base :refer :all]
            [falx.state :as state]
            [falx.point :as pt]
            [falx.rect :as rect]))


(defn draw-blanks!
  ([x y w h]
    (draw-blanks! x y w h 32 32))
  ([x y w h cw ch]
    (dotimes [xx (idiv w cw)]
      (dotimes [yy (idiv h ch)]
        (let [x (+ x (* xx cw))
              y (+ y (* yy ch))]
          (g/draw-point! :blank x y))))))

(defn draw-black!
  [x y w h]
  (g/with-color color/black
                (g/draw-quad! @state/pixel x y w h)))

(defn draw-box!
  ([color x y w h]
    (g/with-color color
                  (draw-box! x y w h)))
  ([x y w h]
    (let [p @state/pixel]
      (g/draw-quad! p x y 1 h)
      (g/draw-quad! p (+ x w -1) y 1 h)
      (g/draw-quad! p x y w 1)
      (g/draw-quad! p x (+ y h -1) w 1))))

(defn draw-filled-box!
  ([color x y w h]
    (draw-black! x y w h)
    (draw-box! color x y w h))
  ([x y w h]
    (draw-filled-box! color/white x y w h)))


(defn draw-hover-text!
  ([text x y]
    (let [[w h] (g/measure-text text)
          w (+ w 8)
          h (+ h 8)]
      (draw-filled-box! color/yellow x y w h)
      (g/draw-text! text (+ x 4) (+ y 17))))
  ([text]
    (let [game @state/game
          [x y] (:mouse-screen game pt/id)
          [_ h] (screen game)
          [_ ch] (cell-size game)
          y (- h y ch)]
      (draw-hover-text! text x y))))

(defn mouse-in?
  "Is the mouse currently in the given rect"
  ([game [x y w h]]
    (mouse-in? game x y w h))
  ([game x y w h]
    (let [[mx my] (:mouse-screen game pt/id)
          [_ sh] (screen game)
          my (- sh my)]
      (rect/pt-in? x y w h mx my))))

(defn draw-button!
  [game sprite text x y w h]
  (draw-box! (if (mouse-in? game x y w h)
               color/green
               color/white) x y w h)
  (g/draw-quad! sprite x y 32 32)
  (g/draw-text! text (+ x 32) (+ y 23)))
