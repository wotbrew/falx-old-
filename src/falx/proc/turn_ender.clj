(ns falx.proc.turn-ender
  (:require [clojure.core.async :as async :refer [go go-loop timeout <!]]
            [clojure.tools.logging :refer [info]]
            [falx.base :refer :all]
            [falx.state :as state])
  (:import (falx.lifecycle ILifecycle)))


(def turn-ender-tick 150)

(defn should-end-player-turn?
  [game]
  (every? #(not (can-act? game %)) (players game)))

(defn maybe-end-player-turn!
  [game]
  (when (should-end-player-turn? game)
    (info "Ending player turn")
    (send state/game next-turn)))

(defn maybe-end-enemy-turn!
  [game]
  (info "Ending enemy turn")
  (send state/game next-turn))

(defn do-end-turns!
  []
  (let [game @state/game]
    (case (mode game)
      :player (maybe-end-player-turn! game)
      :enemy (maybe-end-enemy-turn! game)
      nil)))

(defrecord TurnEnder
  [kill]
  ILifecycle
  (start [this]
    (info "Starting turn ender")
    (reset! kill false)
    (go-loop []
             (if-not @kill
               (do
                 (do-end-turns!)
                 (<! (timeout turn-ender-tick))
                 (recur))
               (info "Stopped turn ender")))
    this)
  (stop [this]
    (info "Stopping turn ender")
    (reset! kill true)
    this))

(defn turn-ender
  []
  (->TurnEnder (atom false)))