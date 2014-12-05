(ns falx.input
  (:import (com.badlogic.gdx Gdx Input$Keys Input$Buttons))
  (:require [clj-tuple :refer [tuple]]
            [clojure.string :as str]))

(defn get-x
  []
  (.. Gdx/input (getX)))

(defn get-y
  []
  (.. Gdx/input (getY)))

(defn key-pressed?
  [key]
  (.. Gdx/input (isKeyPressed key)))

(defn button-pressed?
  [button]
  (.. Gdx/input (isButtonPressed button)))


(defn mouse-pos
  "Find the current mouse position for the given IInput"
  []
  (tuple (get-x) (get-y)))

(def key-up?
  (complement key-pressed?))

(def button-up?
  (complement button-pressed?))

(def key-nums
  (into {}
        (for [n (range 0 10)
              :let [i (+ 7 n)]]
          [(keyword (str "num" n)) i])))

(def key-chars
  (into {}
        (for [n (range 0 26)
              :let [i (+ 65 n)
                    ci (+ 29 n)]]
          [(keyword (str/lower-case (str (char i)))) ci])))

(def key-map
  (merge {:any -1
          :lshift Input$Keys/SHIFT_LEFT
          :rshift Input$Keys/SHIFT_RIGHT
          :esc Input$Keys/ESCAPE
          :space Input$Keys/SPACE
          :f1 Input$Keys/F1
          :f2 Input$Keys/F2
          :f3 Input$Keys/F3
          :f4 Input$Keys/F4
          :f5 Input$Keys/F5
          :f6 Input$Keys/F6
          :f7 Input$Keys/F7
          :f8 Input$Keys/F8
          :f9 Input$Keys/F9
          :f10 Input$Keys/F10
          :f11 Input$Keys/F11
          :f12 Input$Keys/F12}
         key-nums
         key-chars))

(def button-map
  {:left Input$Buttons/LEFT
   :right Input$Buttons/RIGHT})

(defn keys-pressed
  []
  (for [[key real-key] key-map
        :when (key-pressed? real-key)]
    key))

(defn buttons-pressed
  []
  (for [[button real-button] button-map
        :when (button-pressed? real-button)]
    button))

(defn input
  []
  {:pressed (set (concat (buttons-pressed) (keys-pressed)))
   :mouse-pos (mouse-pos)})

(defn hit
  [prev-state]
  (for [p (:pressed prev-state)
        :when (or (when-let [k (get key-map p)] (key-up? k))
                  (when-let [b (get button-map p)] (button-up? b)))]
    p))

(defn next-state
  ([prev-state new-state]
   (assoc new-state
     :hit (hit prev-state))))

(defn debug!
  [input]
  (when-let [hit (seq (:hit input))]
    (println "Input hit" hit)))

(defn commands-hit
  [input key-bindings]
  (let [hit (set (:hit input))
        pressed (set (:pressed input))]
    (for [[mode key command] key-bindings
          :when (case mode
                  :pressed (pressed key)
                  :hit (hit key))]
      command)))

(def default-key-bindings
  "A default set of keybindings to commands that can be used for the game"
  [[:pressed :w, :cam-up]
   [:pressed :a, :cam-left]
   [:pressed :d, :cam-right]
   [:pressed :s, :cam-down]
   [:pressed :lshift, :mod]
   [:hit :left, :primary]
   [:hit :f1, :select-1]
   [:hit :f2, :select-2]
   [:hit :f3, :select-3]
   [:hit :f4, :select-4]
   [:hit :f5, :select-5]
   [:hit :f6, :select-6]])
