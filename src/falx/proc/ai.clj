(ns falx.proc.ai
  (:require
    [falx
     [state :as state :refer :all]
     [base :refer :all]
     [lifecycle :refer :all]]
    [falx.proc
     [eyes :as eyes]]
    [silc.core :refer :all]
    [clojure.tools.logging :refer [debug info]]
    [clojure.core.async :refer [go go-loop <! timeout] :as async]
    [clojure.set :as set]
    [falx.point :as pt]
    [gdx-2d.color :as color]))

;;brain
(def brain-tick 100)
(def step-tick 100)
(def attack-tick 350)
(def brain-spawner-tick 500)

(defn forget-goto!
  [e]
  (send state/game forget-goto e))

(defn bstep!
  [e]
  (go
    (send state/game step-current-path e)
    (<! (timeout step-tick))))

(defn bpath!
  [e goto]
  (let [game @game
        path (path game e goto)]
    (when (not-empty path)
      (send state/game set-path e path))))

(defn bwalk!
  [e]
  (go
    (let [game @game]
      (when-let [goto (att game e :goto)]
        (cond
          (should-cancel-all-movement? game e) (forget-goto! e)
          (current-path-valid? game e) (<! (bstep! e))
          (goto-valid? game e) (bpath! e goto)
          :else (forget-goto! e))))))

(defn pick-random-adjacent-point
  [game e]
  (when-let [pos (pos game e)]
    (rand-nth (pt/adj pos))))

(defn bwalk-random!
  [e]
  (go
    (let [game @game]
      (when (not (att game e :goto))
        (let [n (pick-random-adjacent-point game e)]
          (when (can-move? game e n)
            (send state/game move e n)
            (<! (timeout step-tick))))))))

(defn battack!
  [e]
  (go
    (let [game @game
          adjacent (first (adjacent-hostiles game e))]
      (when (and adjacent (can-attack? game e adjacent))
        (send state/game attack e adjacent)
        (<! (timeout attack-tick))))))

(defn pick-a-target
  [m]
  )

(defn random-bark!
  [e]
  (when (= 1 (rand-int 10))
    (send game #(add-world-text % (entity-world-text % e "Zugzug" color/white)))))

(defn do-brain!
  "Performed on each brain tick"
  [e]
  (go
    (when (can-act? @state/game e)
      (<! (bwalk! e))
      (when true #_(enemy? @state/game e)
        (<! (battack! e))
        (<! (bwalk-random! e))

        #_(random-bark! e)))))

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
      (debug e "now has a brain & eye loop")
      (brain-loop! e)
      (eyes/eye-loop! e))))

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
