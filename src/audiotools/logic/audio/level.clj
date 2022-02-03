(ns audiotools.logic.audio.level
  (:require [schema.core :as s]))

(s/defn level :- (s/maybe s/Num)
  "This is a generic level (dB) function."
  [value     :- s/Num
   reference :- s/Num
   factor    :- s/Num]
  (when (not (zero? reference))
    (-> value
        (/ reference)
        Math/log10
        (* factor))))

(s/defn power->level :- (s/maybe s/Num)
  "Level when the power is expressed as Watts."
  [power :- s/Num]
  (level power 1e-12 10))

(s/defn pressure->level :- (s/maybe s/Num)
  "Level when the pressure is expressed as Pascals."
  [pressure :- s/Num]
  (level pressure 20e-6 20))

(s/defn intensity->level :- (s/maybe s/Num)
  "Level when the intensity is expressed as Watts per square meter."
  [intensity :- s/Num]
  (level intensity 1e-12 10))

(s/defn ^:private value-as-exp :- s/Num
  [level  :- s/Num
   factor :- s/Num]
  (->> factor
       (/ level)
       (Math/pow 10)))

(s/defn ^:private level->value :- s/Num
  [level     :- s/Num
   reference :- s/Num
   factor    :- s/Num]
  (->> factor
       (value-as-exp level)
       (* reference)))

(s/defn level->power :- s/Num
  [level :- s/Num]
  (level->value level 1e-12 10))

(s/defn level->intensity :- s/Num
  [level :- s/Num]
  (level->value level 1e-12 10))

(s/defn level->pressure :- s/Num
  [level :- s/Num]
  (level->value level 20e-6 20))

(s/defn log-sum :- s/Num
  "Sum a list of levels using a log operation."
  [& levels]
  (let [sum-as-exp (fn [sum level] (->> 10 (value-as-exp level) (+ sum)))]
    (->> levels
         (reduce sum-as-exp 0)
         Math/log10
         (* 10))))
