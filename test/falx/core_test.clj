(ns falx.core-test
  (:require [clojure.test :refer :all]
            [falx.core :refer :all]
            [falx.util :refer :all]))

(deftest test-ids
  (testing "I can get the next id of nil as 0"
    (is (= 0 (next-id nil))))
  (testing "I can inc the id counter"
    (is (= 1 (next-id (inc-id nil))))
    (is (= (range 100)
           (take 100 (map next-id (iterate inc-id nil)))))))

(def tiled-map (delay (load-map "test-resources/test-map.json")))

(deftest test-map
  (testing "I can load a map from a file"
    (let [m @tiled-map]
      (testing "name should be test-map"
        (is (= (:name m) :test-map)))
      (testing "size should be 32 x 32"
        (is (= [32 32] (:size m))))
      (testing "I can get a list of tiles / objects"
        (let [tiles (tiles m)
              objects (objects m)
              things (concat tiles objects)]
          (is (every? (comp some? :pos) things) "every tile/object should have a :pos")
          (is (every? (comp #{:test-map} first :pos) things) "every :pos should start with the map name")
          (is (every? (fn [[_ x y]]
                        (and (<= 0 x)
                             (<= 0 y)
                             (< x 32)
                             (< y 32))) (map :pos things)) "every tile/object should be in the bounds of the map")
          (is (every? (comp some? :layer) things) "every tile object should have a :layer")
          (is (every? (comp #{:test-map} first :layer) things) "every :layer should start with the map name")
          (is (every? (comp #{:base
                              :decor
                              :object
                              :item
                              :creature} second :layer) things) "Every :layer should be one of the valid layers"))))))

(deftest test-attr
  (testing "Adding an attribute :foo -> :bar for entity 0"
    (let [m (set-attr nil 0 :foo :bar)]
      (testing "val should be :bar"
        (is (= (attr m 0 :foo)))
        (is (= (attr m 0 :foo :baz)) "else case shouldn't be hit"))
      (testing "should only be :bar for entity 0"
        (is (nil? (attr m 1 :foo))))
      (testing "getting a attr that hasn't been added returns `else` or nil in default case"
        (is (nil? (attr m 0 :baz)))
        (is (= :qux (attr m 0 :baz :qux))))
      (testing "i can update attributes"
        (let [m (set-attr m 0 :something 10)
              m (update-attr m 0 :something + 5)]
          (is (= 15 (attr m 0 :something))))))))

(deftest test-pos-attr
  (testing "place an entity on world :foo"
    (let [m (set-attr nil 0 :pos [:foo 3 4])]
      (testing "should be @ 3, 4 on world :foo"
        (is (= (pt m 0) [3 4]))
        (is (= (pos m 0) [:foo 3 4]))
        (is (= (world m 0) :foo))))))

(deftest test-create-world
  (testing "I can create a world from a tiled-map"
    (let [tm @tiled-map
          m (create-world nil tm)]
      (testing "The world should be the first entity"
        (is (= (attr m 0 :name) :test-map))
        (is (= (attr m 0 :size) [32 32])))
      (testing "Each original tiled-map object should be in the game"
        (is (= (count (map-stream tm))
               (count (by-world m 0)))))
      (testing "I should have a start position"
        (is (not (empty? (by-attr m :type :start))))
        (is (= (count (by-attr m :type :start))
               6)))
      (testing "I should have 5 items"
        (is (= (count (by-attr m :type :item)))
            5))
      (testing "2 doors"
        (is (= (count (by-attr m :type :door)))))
      (testing "2 transition cells"
        (is (= (count (by-attr m :type :transition)))))
      (testing "1 creature"
        (is (= (count (by-attr m :type :creature))))))))

(deftest test-camera
  (let [cam [0 0]
        m {:cam cam :delta 1}]
    (testing "If I moving the camera should respond correctly where it was (y is inverted)"
      (is (south? cam (:cam (apply-command m :cam-up))))
      (is (west? cam (:cam (apply-command m :cam-left))))
      (is (east? cam (:cam (apply-command m :cam-right))))
      (is (north? cam (:cam (apply-command m :cam-down)))))))

(deftest test-create-creature
  (let [m nil
        [m id] (create-pair m {:type :creature})]
    (testing "Lets test some basic properties of an empty creature"
      (is (type= m id :creature))
      (is (creature? m id))
      (is (solid? m id)))
    (testing "If I make a player, the player? fn behaves correctly and is in the players set"
      (is (empty? (players m)) "just double check there aren't already players :)")
      (let [[m id] (create-pair m {:type :creature
                                   :player? true})]
        (is (player? m id))
        (is (contains? (players m) id))))))
