(ns falx.render
  (:import (com.badlogic.gdx.graphics OrthographicCamera))
  (:require [falx.util :refer :all]
            [falx.gfx :refer :all]
            [falx.db :as db]
            [falx.state :as state]
            [falx.cam :as cam]
            [falx.game :as game]
            [clj-tuple :refer [tuple]]))

(defmulti render-layer! (fn [m [wid slice]] slice))

(defn find-region
  [m e]
  (or (db/attr m e :image)
      (db/attr m e :sprite)))

(defmethod render-layer! :default
  [m [wid slice :as layer]]
  (doseq [e (db/by-val m :layer layer)
          :let [[_ x y] (db/attr m e :pos)
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

(defn debug!
  [m]
  (let [[x y] (game/top-left m)]
    (draw-as-text! @state/font
                   (game/debug m)
                   x y)))

(defn render-screen!
  [m]
  (debug! m))

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
            wid (:current-world game 0M)]
        (when-let [cam @state/cam]
          (begin-cam! cam game)
          (render-world! game wid)
          (release-cam! @state/cam)
          (render-screen! game))))))
