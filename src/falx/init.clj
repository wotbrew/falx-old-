(ns falx.init
  (:import (com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont))
  (:require [falx.state :as state]
            [falx.io :as io]
            [falx.loop :as loop]
            [falx.cam :as cam]
            [falx.tiled :as tiled]
            [falx.db :as db]
            [falx.game :as game]))

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

(def default-game
  {:falx.db/ae? #{:sprite :image}
   :falx.db/ave? #{:player? :selected? :world :pos :layer :type}})

(defn game!
  []
  (println "Creating game")
  (let [tmap (tiled/load-map "test-resources/test-map.json")
        game (-> (tiled/create-world default-game tmap)
                 (assoc :current-world 0M)
                 (db/update-with-attr :image state/image->region)
                 (db/update-with-attr :sprite state/sprite->region))]
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
