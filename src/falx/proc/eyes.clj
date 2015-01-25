(ns falx.proc.eyes
  (:require [clojure.core.async :as async :refer [go go-loop timeout <!]]
            [clojure.tools.logging :refer [info debug]]
            [falx.base :refer :all]
            [silc.core :refer :all]
            [falx.state :as state]
            [falx.shapes :as shapes]
            [clojure.set :as set]))

(def eye-tick 200)

(defn do-eyes!
  "Performed on each eye tick"
  [e]
  (go
    (let [game @state/game
          map (att game e :map)
          last-visible (att game e :visible)
          points (visible-points game e)
          now-hidden (set/difference last-visible points)
          now-seen (set/difference points last-visible)]

      (send state/game explore-points e now-seen)
      (send state/game look-at-points e now-seen)
      (send state/game unlook-at-points e now-hidden))))

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
