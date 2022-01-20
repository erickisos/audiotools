(ns audiotools.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [audiotools.core :as subject]))

(deftest main-test
  (is (nil? (subject/-main))))
