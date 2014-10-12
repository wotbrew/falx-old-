(ns falx.io
  (:import (com.badlogic.gdx.graphics.g2d TextureAtlas)
           (com.badlogic.gdx.files FileHandle))
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn atlas
  [file]
  (println "loading atlas..." file)
  (TextureAtlas. (FileHandle. (str file))))

(defn json
  [file]
  (println "loading json..." file)
  (with-open [file (io/reader file)]
    (json/parse-stream file true)))

(defn edn
  [file]
  (println "loading edn..." file)
  (edn/read-string (slurp file)))

