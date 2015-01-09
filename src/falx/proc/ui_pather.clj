(ns falx.proc.ui-pather
  "This component will periodically, when in turn based mode
   assoc the selected entities path to the mouse position into the game state
   in order to draw the pathing flags."
  (:require [falx
             [state :as state :refer :all]
             [base :refer :all]
             [lifecycle :refer :all]]
            [clj-tiny-astar.path :refer [a*]]
            [clojure.core.async :refer [go go-loop timeout <!] :as async]
            [clojure.tools.logging :refer [info debug error]]))

(defn do-ui-path!
  []
  (try
    (if-let [entity (first (selected @game))]
      (send state/game assoc :ui-path (path @game entity (mouse-cell @game)))
      (send state/game dissoc :ui-path))
    (catch Throwable e
      (error "Error associating ui path" e))))

(def ui-pather-tick 500)

(defrecord UiPather
  [kill]
  ILifecycle
  (start [this]
    (info "Starting ui pather")
    (reset! kill false)
    (go-loop []
             (if-not @kill
               (do
                 (do-ui-path!)
                 (<! (timeout ui-pather-tick))
                 (recur))
               (info "Stopped ui pather")))
    this)
  (stop [this]
    (info "Stopping ui pather")
    (reset! kill true)
    this))

(defn ui-pather
  []
  (->UiPather (atom false)))