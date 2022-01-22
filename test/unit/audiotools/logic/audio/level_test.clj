(ns audiotools.logic.audio.level-test
  (:require
   [clojure.test :refer [is testing deftest]]
   [audiotools.logic.audio.level :as audio.level]))

(deftest generic-level
  (testing "if the reference is zero, we can't calculate the level"
    (is (= nil (audio.level/generic-level 0 0.0 0))))
  (testing "if the reference is lower than the value, the level would be positive"
    (is (< 0 (audio.level/generic-level 2 1 0.5))))
  (testing "if the reference is higher than the value, the level would be negative"
    (is (> 0 (audio.level/generic-level 1 2 0.5))))
  (testing "if the reference is equal to the value, the level would be zero"
    (is (zero? (audio.level/generic-level 1 1 0.5)))))

(deftest power-level
  (testing "the power level on a zero power signal is -Inf"
    (is (= ##-Inf (audio.level/power-level 0))))
  (testing "the power level on a positive power signal is positive"
    (is (< 0 (audio.level/power-level 1))))
  (testing "the power level on a negative power signal is not a number"
    (is (Double/isNaN (audio.level/power-level -1)))))

(deftest pressure-level
  (testing "the power level on a zero power signal is -Inf"
    (is (= ##-Inf (audio.level/pressure-level 0))))
  (testing "the power level on a positive power signal is positive"
    (is (< 0 (audio.level/pressure-level 1))))
  (testing "the power level on a negative power signal is not a number"
    (is (Double/isNaN (audio.level/pressure-level -1)))))

(deftest intensity-level
  (testing "the power level on a zero power signal is -Inf"
    (is (= ##-Inf (audio.level/intensity-level 0))))
  (testing "the power level on a positive power signal is positive"
    (is (< 0 (audio.level/intensity-level 1))))
  (testing "the power level on a negative power signal is not a number"
    (is (Double/isNaN (audio.level/intensity-level -1)))))
