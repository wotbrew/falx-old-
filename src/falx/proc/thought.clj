(ns falx.proc.thought
  (:require [falx.state :as state]
            [falx.base :refer :all]
            [silc.core :refer :all]
            [clojure.core.async :refer [go go-loop <! timeout]]))

(def thought-tick 200)

(defn attack-remote-enemy!
  [game e]
  (let [enemies (att game e :visible-enemies)]
    (when (seq enemies)
      (send state/game update-att e :thoughts assoc :move-to-attack (first enemies)))))

(defn attack-adjacent-enemy!
  [game e]
  (let [enemies (adjacent-hostiles game e)
        enemy (first enemies)]
    (when (and enemy (can-attack? game e enemy))
      (send state/game update-att e :thoughts assoc :attack enemy))))

(defn do-thought!
  [e]
  (go
    (let [game @state/game]
      (or (attack-adjacent-enemy! game e)
          (attack-remote-enemy! game e)))))

(defn thought-loop!
  [e]
  (go-loop
    []
    (let [game @state/game
          dead? (or (dead? game e) (not (creature? game e)))]
      (when-not dead?
        (try
          (<! (do-thought! e))
          (catch Throwable e
            (.printStackTrace ^Throwable e)
            (<! (timeout 1000))))
        (<! (timeout thought-tick))
        (recur)))))
