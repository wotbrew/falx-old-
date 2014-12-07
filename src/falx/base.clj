(ns falx.base)

(defprotocol ILifecycle
  (start [this])
  (stop [this]))
