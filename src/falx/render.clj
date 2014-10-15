(ns falx.render
  (:import (com.badlogic.gdx.graphics OrthographicCamera))
  (:require [falx.core :refer :all]
            [falx.util :refer :all]
            [falx.gfx :refer :all]
            [falx.state :as state]
            [falx.cam :as cam]
            [clj-tuple :refer [tuple]]))

(defmulti render-layer! (fn [m [wid slice]] slice))

(defn find-region
  [m e]
  (or (attr m e :image)
      (attr m e :sprite)))

(defmethod render-layer! :default
  [m [wid slice :as layer]]
  (doseq [e (by-attr m :layer layer)
          :let [[x y] (pt m e)
                region (find-region m e)]
          :when (and x y region)]
    (draw-region! region
                  (int (* x 32))
                  (int (* y -32))
                  (int 32)
                  (int 32))))

(defn render-world!
  [m wid]
  (render-layer! m (tuple wid :base))
  (render-layer! m (tuple wid :decor))
  (render-layer! m (tuple wid :object))
  (render-layer! m (tuple wid :creature)))

(defn render-screen!
  [m])

(defn begin-cam!
  [cam game]
  (when-let [[x y] (:cam game)]
    (cam/move! cam x y))
  (cam/update! cam)
  (set-cam! cam))

(defn render!
  []
  (let [tasks (state/read-render-once!)]
    (doseq [t tasks] (t)))
  (when-let [batch @state/batch]
    (clear!)
    (with-batch! batch
      (let [game @state/game
            wid (:world game 0)]
        (when-let [cam @state/cam]
          (begin-cam! cam game)
          (render-world! game wid)
          (release-cam! @state/cam)
          (render-screen! game))))))