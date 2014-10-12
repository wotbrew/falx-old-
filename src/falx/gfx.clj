(ns falx.gfx
  (:import (com.badlogic.gdx.graphics.g2d TextureRegion SpriteBatch)
           (com.badlogic.gdx.graphics GL20 OrthographicCamera)
           (com.badlogic.gdx Gdx)
           (com.badlogic.gdx.math Matrix4)))

(def ^:dynamic ^SpriteBatch *batch* nil)

(defn begin!
  []
  (when *batch*
    (.begin *batch*)))

(defn end!
  []
  (when *batch*
    (.end *batch*)))

(defn set-cam!
  [^OrthographicCamera cam]
  (.setProjectionMatrix *batch* (.combined cam)))

(def identity-matrix
  (Matrix4.))

(defn release-cam!
  [^OrthographicCamera cam]
  (.setTransformMatrix *batch* identity-matrix)
  (.setProjectionMatrix *batch* (.projection cam)))

(defmacro with-batch!
  [batch & forms]
  `(binding [*batch* ~batch]
     (try
       (do
         (begin!)
         ~@forms)
       (finally
         (end!)))))

(defn clear!
  []
  (. Gdx/gl glClearColor 0 0 0 0)
  (. Gdx/gl glClear GL20/GL_COLOR_BUFFER_BIT))

(defn draw-region!
  ([^TextureRegion region x y w h]
   (.draw *batch* region
          ^float x
          ^float y
          ^float w
          ^float h)))
