(ns audiotools.logic.audio.wave-test
  (:require
   [audiotools.logic.audio.wave :as audio.wave]
   [clojure.test :refer [is testing]]
   [schema.test :refer [deftest]]))

(deftest velocity+frequency->wavelength
  (testing "the wavelength on a zero frequency can't be calculated"
    (is (nil? (audio.wave/velocity+frequency->wavelength 0 0))))
  (testing "the wavelength on a positive frequency is positive"
    (is (< 0 (audio.wave/velocity+frequency->wavelength 1 1))))
  (testing "the wavelength on a negative frequency is negative"
    (is (> 0 (audio.wave/velocity+frequency->wavelength -1 1))))
  (testing "the wavelength on a zero velocity is zero"
    (is (= 0 (audio.wave/velocity+frequency->wavelength 0 1)))))

(deftest velocity+wavelength->frequency
  (testing "the frequency on a zero wavelength can't be calculated"
    (is (nil? (audio.wave/velocity+wavelength->frequency 0 0))))
  (testing "the frequency on a positive wavelength is positive"
    (is (< 0 (audio.wave/velocity+wavelength->frequency 1 1))))
  (testing "the frequency on a negative wavelength is negative"
    (is (> 0 (audio.wave/velocity+wavelength->frequency 1 -1))))
  (testing "the frequency on a zero velocity is zero"
    (is (= 0 (audio.wave/velocity+wavelength->frequency 0 1)))))

(deftest wavelength+frequency->velocity
  (testing "the velocity on a zero wavelength is zero"
    (is (zero? (audio.wave/wavelength+frequency->velocity 0 0))))
  (testing "the velocity on a positive wavelength is positive"
    (is (< 0 (audio.wave/wavelength+frequency->velocity 1 1))))
  (testing "the velocity on a negative wavelength is negative"
    (is (> 0 (audio.wave/wavelength+frequency->velocity 1 -1))))
  (testing "the velocity on a zero frequency is zero"
    (is (= 0 (audio.wave/wavelength+frequency->velocity 0 1)))))
