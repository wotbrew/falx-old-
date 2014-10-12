(ns falx.loop
  (:import (com.badlogic.gdx ApplicationListener Gdx)
           (com.badlogic.gdx.backends.lwjgl LwjglApplicationConfiguration LwjglApplication)
           (com.badlogic.gdx.math Vector3))
  (:require [falx.render :as render]
            [falx.state :as state]
            [falx.input :as input]
            [falx.core :refer :all]
            [falx.cam :as cam]))

(def key-bindings
  [[:pressed :w, :cam-up]
   [:pressed :a, :cam-left]
   [:pressed :d, :cam-right]
   [:pressed :s, :cam-down]
   [:pressed :lshift, :cam-fast]
   [:hit :left, :select]])

(defn grender
  []
  (render/render!)
  (when-let [cam @state/cam]
    (let [next (input/input)
          input (swap! state/input input/next-state next)
          mouse (:mouse-pos input)
          world-mouse (cam/unproject cam mouse)
          commands (input/commands-hit input key-bindings)
          delta (.. Gdx/graphics (getDeltaTime))
          fps (.. Gdx/graphics (getFramesPerSecond))]
      (swap! state/game #(-> %
                             (assoc :world-mouse world-mouse
                                    :screen-mouse mouse
                                    :delta delta
                                    :fps fps)
                             (apply-commands commands))))))

(defn gresize
  [x y]
  )

(defn gcreate
  []
  )

(def listener
  (proxy
      [ApplicationListener]
      []
    (pause [])
    (resume [])
    (dispose [])
    (create []
      (try
        (gcreate)
        (catch Throwable e
          (println e))))
    (render []
      (try
        (grender)
        (catch Throwable e
          (println e))))
    (resize [x y]
      (try
        (gresize x y)
        (catch Throwable e
          (println e))))))

(defn loop!
  ([]
   (loop! nil))
  ([settings]
   (let [cfg (LwjglApplicationConfiguration.)]
     (set! (. cfg width) (get settings :width 1024))
     (set! (. cfg height) (get settings :height 768))
     (set! (. cfg fullscreen) (get settings :fullscreen? false))
     (set! (. cfg title) "Falx 0.1.0")
     (set! (. cfg vSyncEnabled) (get settings :vsync? false))
     (set! (. cfg foregroundFPS) (get settings :max-fps 60))
     (set! (. cfg backgroundFPS) (get settings :max-fps 60))
     (set! (. cfg resizable) false)
     (future (LwjglApplication. ^ApplicationListener listener cfg)))))

(comment
  "Start the main game loop, libgdx cannot be restarted without restarting the jvm"
  (loop!))