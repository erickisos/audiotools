(ns audiotools.logic.audio.level-test
  (:require [clojure.test :refer [is testing deftest]]
            [audiotools.logic.audio.level :as audio.level]))

(deftest level
  (testing "if the reference is zero, we can't calculate the level"
    (is (= nil (audio.level/level 0 0.0 0))))
  (testing "if the reference is lower than the value, the level would be positive"
    (is (< 0 (audio.level/level 2 1 0.5))))
  (testing "if the reference is higher than the value, the level would be negative"
    (is (> 0 (audio.level/level 1 2 0.5))))
  (testing "if the reference is equal to the value, the level would be zero"
    (is (zero? (audio.level/level 1 1 0.5)))))

(deftest power->level
  (testing "the power level on a zero power signal is -Inf"
    (is (= ##-Inf (audio.level/power->level 0))))
  (testing "the power level on a positive power signal is positive"
    (is (< 0 (audio.level/power->level 1))))
  (testing "the power level on a negative power signal is not a number"
    (is (Double/isNaN (audio.level/power->level -1)))))

(deftest pressure->level
  (testing "the power level on a zero power signal is -Inf"
    (is (= ##-Inf (audio.level/pressure->level 0))))
  (testing "the power level on a positive power signal is positive"
    (is (< 0 (audio.level/pressure->level 1))))
  (testing "the power level on a negative power signal is not a number"
    (is (Double/isNaN (audio.level/pressure->level -1)))))

(deftest intensity->level
  (testing "the power level on a zero power signal is -Inf"
    (is (= ##-Inf (audio.level/intensity->level 0))))
  (testing "the power level on a positive power signal is positive"
    (is (< 0 (audio.level/intensity->level 1))))
  (testing "the power level on a negative power signal is not a number"
    (is (Double/isNaN (audio.level/intensity->level -1)))))

(deftest level->power
  (testing "the power on a zero level is the reference"
    (is (= 1e-12 (audio.level/level->power 0))))
  (testing "the power on a 120 level is 1"
    (is (= 1.0 (audio.level/level->power 120))))
  (testing "the power on a positive level is positive"
    (is (< 0 (audio.level/level->power 1))))
  (testing "the power on a negative level it's still a positive number"
    (is (< 0 (audio.level/level->power -1)))))

(deftest level->intensity
  (testing "the power on a zero level is the reference"
    (is (= 1e-12 (audio.level/level->intensity 0))))
  (testing "the power on a 120 level is 1"
    (is (= 1.0 (audio.level/level->intensity 120))))
  (testing "the power on a positive level is positive"
    (is (< 0 (audio.level/level->intensity 1))))
  (testing "the power on a negative level it's still a positive number"
    (is (< 0 (audio.level/level->intensity -1)))))

(deftest level->pressure
  (testing "the power on a zero level is the reference"
    (is (= 20e-6 (audio.level/level->pressure 0))))
  (testing "the power on a 120 level is 20"
    (is (= 20.0 (audio.level/level->pressure 120))))
  (testing "the power on a positive level is positive"
    (is (< 0 (audio.level/level->pressure 1))))
  (testing "the power on a negative level it's still a positive number"
    (is (< 0 (audio.level/level->pressure -1)))))

(deftest log-sum
  (testing "one zero level is zero"
    (is (zero? (audio.level/log-sum 0))))
  (testing "the result of a sum of levels is absolute"
    (is (< 0 (audio.level/log-sum -1 -2 -3 -4 -5)))
    (is (< 0 (audio.level/log-sum 1 2 3 4 5))))
  (testing "the result of a sum of levels is never bigger than the sum of the min and the max value + 3"
    (let [values    (-> (rand-int 100)
                        (+ 1)
                        (take (repeatedly #(rand-int 1000))))
          min-value (apply min values)
          max-value (apply max values)]
      (is (> (+ min-value max-value 3) (apply audio.level/log-sum values))))))
