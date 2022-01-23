(ns audiotools.logic.audio.level
  (:require [schema.core :as s]))

(s/defn generic-level :- s/Num
  [value     :- s/Num
   reference :- s/Num
   factor    :- s/Num]
  (when (not (zero? reference))
    (-> value
        (/ reference)
        Math/log10
        (* factor))))

(s/defn power-level :- s/Num
  [power :- s/Num]
  (generic-level power 1e-12 10))

(s/defn pressure-level :- s/Num
  [pressure :- s/Num]
  (generic-level pressure 20e-6 20))

(s/defn intensity-level :- s/Num
  [intensity :- s/Num]
  (generic-level intensity 1e-12 10))

(s/defn level->power :- s/Num
  [level :- s/Num]
  (->> 10
       (/ level)
       (Math/pow 10)
       (* 1e-12)))

(s/defn level->intensity :- s/Num
  [level :- s/Num]
  (->> 10
       (/ level)
       (Math/pow 10)
       (* 1e-12)))

(s/defn level->pressure :- s/Num
  [level :- s/Num]
  (->> 20
       (/ level)
       (Math/pow 10)
       (* 20e-6)))
