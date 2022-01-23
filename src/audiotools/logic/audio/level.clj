(ns audiotools.logic.audio.level
  (:require [schema.core :as s]))

(s/defn level :- s/Num
  "This is a generic level (dB) function."
  [value     :- s/Num
   reference :- s/Num
   factor    :- s/Num]
  (when (not (zero? reference))
    (-> value
        (/ reference)
        Math/log10
        (* factor))))

(s/defn power->level :- s/Num
  "Level when the power is expressed as Watts."
  [power :- s/Num]
  (level power 1e-12 10))

(s/defn pressure->level :- s/Num
  "Level when the pressure is expressed as Pascals."
  [pressure :- s/Num]
  (level pressure 20e-6 20))

(s/defn intensity->level :- s/Num
  "Level when the intensity is expressed as Watts per square meter."
  [intensity :- s/Num]
  (level intensity 1e-12 10))

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

(s/defn sum-levels :- s/Num
  "Sum a list of levels."
  [& levels]
  (->> levels
       (map #(/ % 10))
       (map #(Math/pow 10 %))
       (apply +)
       (Math/log10)
       (* 10)))
