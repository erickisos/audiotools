(ns audiotools.logic.math-test
  (:require [audiotools.logic.math :as logic.math]
            [schema.test :refer [deftest]]
            [clojure.test :refer [testing is]]))

(deftest circle-area
  (testing "The area of a circle with a radius of 1 is 12.566370614359172"
    (is (= 12.566370614359172 (logic.math/circle-area 1)))))

(deftest circle-radius
  (testing "The radius of a circle with an area of 12.566370614359172 is 1"
    (is (= 1.0 (logic.math/circle-radius 12.566370614359172)))))
