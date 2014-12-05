(ns falx.core-test
  (:require [falx.core :refer :all]
            [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [falx.point :as pt]))

(deftest test-moving-cam
  (testing "create an initial game with a unit delta"
    (let [game {:delta 1}]
      (testing "moving the cam up, left, right and down from an initial state"
        (are [x y] (y (:cam (apply-command game x)) (:cam game pt/id))
          :cam-up pt/north?
          :cam-left pt/east?
          :cam-right pt/west?
          :cam-down pt/south?))
      (testing "moving the cam in multiple directions at at time"
        (are [x y] (y (:cam (apply-commands game x)) (:cam game pt/id))
          [:cam-up :cam-left] pt/north-east?
          [:cam-up :cam-right] pt/north-west?
          [:cam-up :cam-up] pt/north?
          [:cam-down :cam-left] pt/south-east?
          [:cam-down :cam-right] pt/south-west?
          [:cam-left :cam-right] =
          [:cam-up :cam-down :cam-left :cam-right] =))
      (testing "moving the cam up, left, right and down from an existing cam"
        (let [game (assoc game :cam [1000 -500])]
          (are [x y] (y (:cam (apply-command game x)) (:cam game pt/id))
            :cam-up pt/north?
            :cam-left pt/east?
            :cam-right pt/west?
            :cam-down pt/south?))))))

