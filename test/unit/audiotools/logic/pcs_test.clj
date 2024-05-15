(ns unit.audiotools.logic.pcs-test
  (:require [clojure.test :refer [is testing]]
            [schema.test :refer [deftest]]
            [audiotools.logic.pcs :as pcs]))

(deftest pcs->notes
  (testing "The pitch class set for '123AB' is (1 2 3 10 11)"
    (is (= [1 2 3 10 11] (pcs/pcs->notes "123AB")))
    (is (= [3 2 1] (pcs/pcs->notes "321")))))

(deftest distance
  (testing "The distance between two notes"
    (is (= 10 (pcs/distance 1 11)))
    (is (= 10 (pcs/distance 3 1)))
    (is (= 6 (pcs/distance 10 4)))))

(deftest pcs->permutations
  (testing ""
    (is (= ["134AB"
            "34AB1"
            "4AB13"
            "AB134"
            "B134A"] (pcs/pcs->permutations "134AB")))))

(deftest pcs->permutations-map
  (testing ""
    (is (= {"134AB" 10
            "34AB1" 10
            "4AB13" 11
            "AB134" 6
            "B134A" 11} (pcs/pcs->permutations-map "134AB")))))

(deftest pcs->ideal-form
  (testing ""
    (is (= "AB134" (pcs/pcs->ideal-form "134AB"))))

  (testing ""
    (is (= "AB134" (pcs/pcs->ideal-form "AB134")))))

(deftest pcs->inversion
  (testing "can invert a simple PCS"
    (is (= "21B98" (pcs/pcs->inversion "AB134")))))

(deftest pcs->prime-form
  (testing "can calculate the correct prime form"
    (is (= "1B982" (pcs/pcs->basic-form "134AB")))
    (is (= "1B982" (pcs/pcs->basic-form "AB134")))))
