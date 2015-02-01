(ns falx.proc.eyes
  (:require [clojure.core.async :as async :refer [go go-loop timeout <!]]
            [clojure.tools.logging :refer [info debug]]
            [falx.base :refer :all]
            [silc.core :refer :all]
            [falx.state :as state]
            [falx.shapes :as shapes]
            [clojure.set :as set]
            [clojure.data.int-map :as im]))

(def eye-tick 200)

(defn just-do-eyes!
  [game e]
  (let [map (att game e :map)
        last-points (att game e :visible-points)

        points (into #{} (find-visible-points game e))
        entities (into (im/int-set) (find-entities-in game map points))

        now-seen-points (set/difference points last-points)]

        (send state/game set-att e
              :explored-points (into (att game e :explored-points #{}) now-seen-points)
              :visible-points points
              :visible-entities entities)))

(defn should-do-eyes?
  [game e]
  (on-player-map? game e))

(defn do-eyes!
  "Performed on each eye tick"
  [e]
  (go
    (let [game @state/game]
      (if (should-do-eyes? game e)
        (just-do-eyes! game e)
        (<! (timeout 1000))))))

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
