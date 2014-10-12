(ns falx.init
  (:import (com.badlogic.gdx.graphics.g2d SpriteBatch))
  (:require [falx.state :as state]
            [falx.io :as io]
            [falx.loop :as loop]
            [falx.cam :as cam]
            [falx.core :refer :all]))

(defn atlas!
  []
  (println "Creating atlas")
  (reset! state/atlas @(state/on-renderer (io/atlas "resources/tiles/main.pack"))))

(defn batch!
  []
  (println "Creating sprite batch")
  (reset! state/batch @(state/on-renderer (SpriteBatch.))))

(defn cam!
  []
  (println "Creating camera")
  (reset! state/cam (cam/camera 1024 768)))

(defn game!
  []
  (println "Creating game")
  (let [tmap (load-map "test-resources/test-map.json")
        game (create-world nil tmap)
        game (update-attrs game :image state/image->region)]
    (reset! state/game game)
    nil))

(comment
  (loop/loop!)
  (atlas!)
  (batch!)
  (cam!)
  (game!))