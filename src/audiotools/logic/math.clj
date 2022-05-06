(ns audiotools.logic.math
  (:require [schema.core :as s]))

(s/defn circle-area :- s/Num
  [radius :- s/Num]
  (-> radius
      (Math/pow 2)
      (* 4 Math/PI)))

(s/defn radius :- s/Num
  [area :- s/Num]
  (-> area
      (/ 4 Math/PI)
      Math/sqrt))
