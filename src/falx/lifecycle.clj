(ns falx.lifecycle)

(defprotocol ILifecycle
  (start [this])
  (stop [this]))

