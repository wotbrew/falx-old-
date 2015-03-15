(ns falx.proc.act
  (:require [falx.state :as state]
            [falx.base :refer :all]
            [falx.point :as pt]
            [silc.core :refer :all]
            [clojure.core.async :refer [go go-loop <! timeout]]))

(def act-tick 200)
(def attack-tick 500)
(def step-tick 0)

(defn forget-goto!
  [e]
  (send state/game forget-goto e))

(defn do-step!
  [e]
  (go
    (send state/game step-current-path e)
    (<! (timeout step-tick))
    true))

(defn find-path
  [game e goto]
  (let [path (path game e goto)]
    (when (not-empty path)
      (send state/game set-path e path))))

(defn do-walk!
  [game e goto]
  (go
    (cond
     (should-cancel-all-movement? game e) (forget-goto! e)
     (current-path-valid? game e) (<! (do-step! e))
     (goto-valid? game e) (find-path game e goto)
     :else (forget-goto! e))))

(defn do-move-to-target!
  [game e target]
  (let [fadj (nearest-adjacent game e target)]
    (if (and (<= 2 (current-ap game e))
             (not (adjacent? game e target)))
      (when (and fadj (not= (:goto (att game e :thoughts)) fadj))
        (send state/game goto e fadj))
      (do (forget-goto! e)
          false))))

(defn do-attack!
  [game e target]
  (go
    (when (can-attack? game e target)
      (send state/game attack e target)
      (<! (timeout attack-tick))
      true)))

(defn forfit!
  [e]
  (send state/game set-ap e 0))

(defn do-non-player-actions!
  [game e thoughts]
  (go
    (let [attack (:attack thoughts)
          move-to (:move-to-attack thoughts)]
      (or (and attack (<! (do-attack! game e attack)))
          (and move-to (do-move-to-target! game e move-to))))))

(defn do-interact!
  [game e target]
  (if (not (adjacent? game e target))
    (do-move-to-target! game e target)
    (send state/game interact e target)))

(defn do-act!
  [game e]
  (go
    (when (can-act? game e)
      (let [thoughts (att game e :thoughts)
            player? (player? game e)]
        (or (and (not player?) (<! (do-non-player-actions! game e thoughts)))
            (when-let [goto (:goto thoughts)]
              (<! (do-walk! game e goto)))
            (when-let [interact (:interact thoughts)]
              (do-interact! game e interact))
            (and (not player?) (forfit! e)))))))

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
