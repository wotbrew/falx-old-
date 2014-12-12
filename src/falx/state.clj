(ns falx.state
  (:import (com.badlogic.gdx.graphics.g2d TextureAtlas TextureRegion)
           (com.badlogic.gdx.files FileHandle)
           (com.badlogic.gdx.graphics OrthographicCamera))
  (:require [gdx-2d.core :as g]
            [gdx-loopy.core :refer [on-render-thread]]
            [silc.core :refer :all]
            [clojure.tools.logging :refer [info debug error]]))

(def default-game
  "The initial value of the game"
  (->
    {}
    (enable-ae-indexing)
    (with-indexes :player? :enemy? :type)
    (with-composite-indexes #{:map :layer}
                            #{:map :pos})))

(defonce ^{:doc "The current game"} game (agent default-game))
(defonce ^{:doc "The set of all concious entities"} conscious (ref #{}))
(defonce ^{:doc "The last input state"} input (atom nil))
(defonce ^{:doc "The default font"} font (atom nil))
(defonce ^{:doc "The global sprite batch"} batch (atom nil))
(defonce ^{:doc "The global texture atlas"} atlas (atom nil))
(defonce ^{:doc "A cache of regions against files and subregions against keys of the form [\"file\" x y w h]"}
         region-cache
  (atom nil))
(defonce ^{:doc "A global 'pixel' texture for drawing solid color"}
         pixel (atom nil))

(def settings
  "A map of default settings read in from resources/settings.edn"
  (read-string (slurp "resources/settings.edn")))

(def default-size
  "Get the screen size by default"
  [(:width settings) (:height settings)])

(defonce
  ^{:doc  "The global camera"}
  cam
  (atom nil))

(def sprites
  "Contains a map of sprites to their sub region keys read in from resources/sprites.edn"
  (read-string (slurp "resources/sprites.edn")))

(defn cache-region!
  "Cache a given region at the given key"
  [key region]
  (info "Caching region" key)
  (swap! region-cache assoc key region)
  region)

(defn find-file-region
  "Finds a region from the global atlas for the given file - caches once found."
  [file]
  (if-let [existing (get @region-cache file)]
    existing
    (let [region (.findRegion ^TextureAtlas @atlas file)]
      (cache-region! file region))))

(defn find-sub-region
  "Finds a sub region in the atlas - caches once found."
  [file x y w h]
  (let [key [file x y w h]
        existing (get @region-cache key)]
    (if existing
      existing
      (let [region (find-file-region file)
            sub (g/texture-region region x y w h)]
        (cache-region! key sub)))))

(defn find-sprite
  "Finds a sprite by looking up its representation first in the sprite map
   and then in the region cache. This works for keys of the form
   :some-keyword
   [\"file\" [x y w h]]"
  [key]
  (let [[file [x y w h]] (let [spr (get sprites key key)]
                           (if (sequential? spr)
                             spr
                             nil))]
    (when file
      (find-sub-region file x y w h))))

(extend-protocol g/IDrawQuad
  ;;by default we can draw a rect for anything as long as it is resolvable to a sprite
  Object
  (draw-quad! [this x y w h]
    (when-let [spr (find-sprite this)]
      (g/draw-quad! spr x y w h))))

(extend-protocol g/IDrawPoint
  ;;by default we can draw a rect for anything as long as it is resolvable to a sprite
  Object
  (draw-point! [this x y]
    (when-let [spr (find-sprite this)]
      (g/draw-point! spr x y))))


(defn init!
  "Initialises the game"
  []
  (on-render-thread
    (send game (constantly default-game))
    (reset! input nil)
    (reset! font (g/bitmap-font))
    (reset! batch (g/batch))
    (reset! atlas (g/atlas "resources/tiles/main.pack"))
    (reset! pixel (g/pixel))
    (reset! cam (g/ortho-cam (:width settings) (:height settings)))))

