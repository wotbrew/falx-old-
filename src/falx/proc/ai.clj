(ns falx.proc.ai
  (:require
    [falx
     [state :as state :refer :all]
     [base :refer :all]
     [lifecycle :refer :all]]
    [falx.proc
     [eyes :as eyes]
     [interpret :as interpret]
     [thought :as thought]
     [act :as act]]
    [silc.core :refer :all]
    [clojure.tools.logging :refer [debug info]]
    [clojure.core.async :refer [go go-loop <! timeout] :as async]
    [clojure.set :as set]
    [falx.point :as pt]
    [gdx-2d.color :as color]))

;;brain
(def brain-tick 100)
(def brain-spawner-tick 500)


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
      (eyes/eye-loop! e)
      (interpret/interpret-loop! e)
      (thought/thought-loop! e)
      (act/act-loop! e))))

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
