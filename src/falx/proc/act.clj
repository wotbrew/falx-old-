(ns falx.proc.act
  (:require [falx.state :as state]
            [falx.base :refer :all]
            [falx.point :as pt]
            [silc.core :refer :all]
            [clojure.core.async :refer [go go-loop <! timeout]]))

(def act-tick 200)

(defn do-act!
  [game e]
  (go
    (when (and (can-act? game e)
               (not (player? game e)))
      (let [thoughts (att game e :thoughts)]
        (when-let [target (:attack thoughts)]
          (let [adjacent (adjacent-points game target)
                fadj (first (filter #(not (solid-at? game %)) adjacent))]
            (if (and (<= 2 (current-ap game e))
                     (not (adjacent? game e target)))
              (when fadj
                (send state/game goto e fadj))
              (send state/game forget-goto e))))))))



(defn act-loop!
  [e]
  (go-loop
    []
    (let [game @state/game
          dead? (or (dead? game e) (not (creature? game e)))]
      (when-not dead?
        (try
          (<! (do-act! game e))
          (catch Throwable e
            (.printStackTrace ^Throwable e)
            (<! (timeout 1000))))
        (<! (timeout act-tick))
        (recur)))))
