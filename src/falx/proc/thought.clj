(ns falx.proc.thought
  (:require [falx.state :as state]
            [falx.base :refer :all]
            [silc.core :refer :all]
            [clojure.core.async :refer [go go-loop <! timeout]]))

(def thought-tick 200)

(defn do-thought!
  [e]
  (go
    (let [game @state/game
          enemies (att game e :visible-enemies)]
     (send state/game update-att e :thoughts assoc :attack (first enemies)))))

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
