(ns falx.cam
  (:require [clj-tuple :refer [tuple]])
  (:import (com.badlogic.gdx.graphics OrthographicCamera Camera)
           (com.badlogic.gdx.math Vector3)))

(defn camera
  ([]
   (OrthographicCamera.))
  ([width height]
   (OrthographicCamera. width height)))

(defn move!
  [^OrthographicCamera cam x y]
  (.set (.position cam)
        x
        y
        0))

(defn update!
  [^OrthographicCamera cam]
  (.update cam))

(defn project
  ([cam [x y]]
   (project cam x y))
  ([^Camera cam x y]
   (let [vec (Vector3. x y 1)]
     (.project cam vec)
     (tuple
       (int (.x vec))
       (int (.y vec))))))

(defn unproject
  ([cam [x y]]
   (unproject cam x y))
  ([^Camera cam x y]
   (let [vec (Vector3. x y 1)]
     (.unproject cam vec)
     (tuple
       (int (.x vec))
       (int (.y vec))))))

