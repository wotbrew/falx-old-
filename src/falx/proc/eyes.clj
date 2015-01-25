(ns falx.proc.eyes
  (:require [clojure.core.async :as async :refer [go go-loop timeout <!]]
            [clojure.tools.logging :refer [info debug]]
            [falx.base :refer :all]
            [silc.core :refer :all]
            [falx.state :as state]
            [falx.shapes :as shapes]))

(def eye-tick 200)

(defn do-eyes!
  "Performed on each eye tick"
  [e]
  (go
    (let [game @state/game
          map (att game e :map)
          points (visible-points game e)]
      (when (player? game e)
        (send state/game explore-points e points)))))

(defn eye-loop!
  [e]
  (go-loop
    []
    (let [game @state/game
          dead? (or (dead? game e) (not (creature? game e)))]
      (when-not dead?
        (try
          (<! (do-eyes! e))
          (catch Throwable e
            (.printStackTrace e)
            (<! (timeout 1000))))
        (<! (timeout eye-tick))
        (recur)))))
