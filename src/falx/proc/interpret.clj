(ns falx.proc.interpret
  (:require [falx.state :as state]
            [falx.base :refer :all]
            [silc.core :refer :all]
            [clojure.core.async :refer [go go-loop <! timeout]]))


(def interpret-tick 500)

(defn do-interpret!
  [e]
  (go
    (let [game @state/game
          visible (att game e :visible-entities)
          enemies (filter #(hostile-to? game e %) visible)]
      (send state/game set-att e :visible-enemies enemies))))

(defn interpret-loop!
  [e]
  (go-loop
    []
    (let [game @state/game
          dead? (or (dead? game e) (not (creature? game e)))]
      (when-not dead?
        (try
          (<! (do-interpret! e))
          (catch Throwable e
            (.printStackTrace ^Throwable e)
            (<! (timeout 1000))))
        (<! (timeout interpret-tick))
        (recur)))))
