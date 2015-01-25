(ns falx.proc.ai
  (:require
    [falx
     [state :as state :refer :all]
     [base :refer :all]
     [lifecycle :refer :all]]
    [silc.core :refer :all]
    [clojure.tools.logging :refer [debug info]]
    [clojure.core.async :refer [go go-loop <! timeout] :as async]
    [clojure.set :as set]
    [falx.point :as pt]))

;;brain
(def brain-tick 100)
(def brain-spawner-tick 500)

(defn forget-goto!
  [e]
  (send state/game forget-goto e))

(defn bstep!
  [e]
  (send state/game step-current-path e))

(defn bpath!
  [e goto]
  (let [game @game
        path (path game e goto)]
    (when (not-empty path)
      (send state/game set-path e path))))

(defn bwalk!
  [e]
  (let [game @game]
    (when-let [goto (att game e :goto)]
      (cond
        (should-cancel-all-movement? game e) (forget-goto! e)
        (current-path-valid? game e) (bstep! e)
        (goto-valid? game e) (bpath! e goto)
        :else (forget-goto! e)))))

(defn pick-random-adjacent-point
  [game e]
  (when-let [pos (pos game e)]
    (rand-nth (pt/adj pos))))

(defn bwalk-random!
  [e]
  (let [game @game]
    (when (not (att game e :goto))
      (let [n (pick-random-adjacent-point game e)]
        (when (can-move? game e n)
          (send state/game goto e n))))))


(defn do-brain!
  "Performed on each brain tick"
  [e]
  (go
    (when (can-act? @state/game e)
      (bwalk! e)
      (when (enemy? @state/game e)
        (bwalk-random! e)))))

(defn brain-loop!
  [e]
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
        (recur)))))

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
      (brain-loop! e))))

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
