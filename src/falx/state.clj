(ns falx.state
  (:import (com.badlogic.gdx.graphics.g2d TextureRegion TextureAtlas)))

(defonce input (atom nil))

(defonce render-once (ref []))

(defn read-render-once!
  []
  (when-not (empty? @render-once)
    (dosync
      (let [all @render-once]
        (ref-set render-once [])
        all))))

(defn put-render-once!
  [f]
  (dosync
    (alter render-once conj f)))

(defn on-renderer-call
  [f]
  (let [p (promise)]
    (put-render-once! #(deliver p (f)))
    p))

(defmacro on-renderer
  [& body]
  `(on-renderer-call #(do ~@body)))

(defonce region-cache (atom nil))

(defn get-cached-region
  [key]
  (get @region-cache key))

(defn cache-region
  [key region]
  (println "Caching region" key)
  (swap! region-cache assoc key region)
  region)

(defonce atlas (atom nil))

(defn sub-region
  [^TextureRegion region [x y w h :as rect]]
  (TextureRegion. region
                  (int x)
                  (int y)
                  (int w)
                  (int h)))

(defn ^TextureRegion get-region
  ([file]
   (if-let [region (get-cached-region file)]
     region
     (when-let [atlas @atlas]
       (cache-region file (.findRegion ^TextureAtlas atlas file)))))
  ([file rect]
   (if-let [region (get-cached-region [file rect])]
     region
     (when-let [region (get-region file)]
       (cache-region [file rect] (sub-region region rect))))))

(defn atlas-file
  [file]
  (second (re-find #"\/(.+)\." file)))

(defn image->region
  [image]
  (get-region (-> image :file atlas-file)
              (-> image :rect)))

(defonce sprites (atom nil))

(defn sprite->region
  [sprite]
  (when-let [[file rect] (get @sprites sprite)]
    (get-region file rect)))

(defonce font (atom nil))

(defonce batch (atom nil))

(defonce cam (atom nil))

(defonce game (atom nil))
