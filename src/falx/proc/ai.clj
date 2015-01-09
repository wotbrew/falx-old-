(ns falx.proc.ai
  (:require
    [falx
     [state :as state :refer :all]
     [base :refer :all]
     [lifecycle :refer :all]]
    [silc.core :refer :all]
    [clojure.tools.logging :refer [debug info]]
    [clojure.core.async :refer [go go-loop <! timeout] :as async]
    [clojure.set :as set]))

;;brain
(def walk-tick 125)
(def brain-tick 100)
(def brain-spawner-tick 500)


(declare bwalk!)

(defn forget-goto!
  [e]
  (send state/game forget-goto e))

(defn attempt-move!
  [e next]
  (go (let [game @game
            dead? (dead? game e)]
        (if dead?
          (do (debug e "is dead - cancelling move")
              (forget-goto! e))
          (do (send state/game move e next)
              (await state/game)
              (<! (timeout walk-tick)))))))

(defn attempt-walk!
  [e path]
  (let [target (last path)]
    (go-loop
      [[p & rest] path]
      (cond
        (not p) (debug e "is done walking")
        (not= target (att @game e :goto)) (debug e "goto target changed - repathing")
        :else (do
                (debug e "moving to" p)
                (<! (attempt-move! e p))
                (if (not= (pos @game e) p)
                  (debug e "could not move due to unforseen obstacle - repathing")
                  (recur rest)))))))

(defn bwalk-at-goal!
  [e]
  (debug e "is at its goal")
  (send state/game forget-goto e)
  (await state/game))

(defn bwalk-goto-now-solid!
  [goto e]
  (debug e "can no longer move to" goto "- it is solid")
  (forget-goto! e))

(defn bwalk!
  [e]
  (go
    (let [game @game
          goto (att game e :goto)]
      (cond
        (not goto) true
        (= goto (pos game e)) (bwalk-at-goal! e)
        (solid-at? game goto) (bwalk-goto-now-solid! goto e)
        :else (if-let [pa (path game e goto)]
                (do
                  (debug e "path to" goto "is" pa)
                  (<! (attempt-walk! e pa)))
                (forget-goto! e))))))



(defn do-brain!
  "Performed on each brain tick"
  [e]
  (go
    (<! (bwalk! e))))

(defn do-brain-spawning!
  []
  (let [creatures (creatures @game)
        new
        (dosync
          (let [new (set/difference creatures @conscious)]
            (commute conscious into new)
            new))]
    (doseq [e new]
      (debug e "now has a brain")
      (go-loop
        []
        (let [game @game
              dead? (or (dead? game e) (not (creature? game e)))]
          (when dead?
            (debug e "is dead - removing brain")
            (dosync (commute conscious disj e)))
          (when-not dead?
            (try
              (<! (do-brain! e))
              (catch Throwable e
                (.printStackTrace e)
                (<! (timeout 1000))))
            (<! (timeout brain-tick))
            (recur)))))))

(defrecord BrainSpawner
  [kill]
  ILifecycle
  (start [this]
    (info "Starting brain spawner")
    (reset! kill false)
    (go-loop []
             (if-not @kill
               (do
                 (do-brain-spawning!)
                 (<! (timeout brain-spawner-tick))
                 (recur))
               (info "Stopped brain spawner")))
    this)
  (stop [this]
    (info "Stopping brain spawner")
    (reset! kill true)
    this))

(defn brain-spawner
  "A brain spawner is a ILifecycle component
   that will when started periodically start ai brains
   for entities where appropriate."
  []
  (->BrainSpawner (atom false)))
