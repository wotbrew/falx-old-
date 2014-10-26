(ns falx.init
  (:import (com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont))
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
  (reset! state/cam (cam/camera 1024 768))
  (state/on-renderer (cam/move! @state/cam 0 0)))

(defn sprites!
  []
  (println "Creating sprites")
  (reset! state/sprites (io/edn "resources/tiles/sprites.edn"))
  nil)

(defn font!
  []
  (println "Creating font")
  (reset! state/font @(state/on-renderer (BitmapFont.))))

(defn game!
  []
  (println "Creating game")
  (let [tmap (load-map "test-resources/test-map.json")
        game (-> (create-world nil tmap)
                 (assoc :current-world 0)
                 (update-attrs :image state/image->region)
                 (update-attrs :sprite state/sprite->region))]
    (reset! state/game game)
    nil))

(comment
  (loop/loop!)
  (atlas!)
  (batch!)
  (cam!)
  (sprites!)
  (font!)
  (game!))
