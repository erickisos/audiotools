(ns audiotools.logic.audio.level-test
  (:require
   [clojure.test :refer [is testing deftest]]
   [audiotools.logic.audio.level :as audio.level]))

(deftest generic-level
  (testing "if the reference is zero, we can't calculate the level"
    (is (= nil (audio.level/generic-level 0 0.0 0))))
  (testing "if the reference is lower than the value, the level would be positive"
    (is (< 0.0 (audio.level/generic-level 2 1 0.5))))
  (testing "if the reference is higher than the value, the level would be negative"
    (is (> 0.0 (audio.level/generic-level 1 2 0.5))))
  (testing "if the reference is equal to the value, the level would be zero"
    (is (= 0.0 (audio.level/generic-level 1 1 0.5)))))
