(ns audiotools.core-test
  (:require
   [audiotools.core :as subject]
   [clojure.test :refer [deftest is]]))

(deftest main-test
  (is (nil? (subject/-main))))
