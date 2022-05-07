(ns audiotools.logic.audio.wave
  (:require [schema.core :as s]))

(s/defn velocity+frequency->wavelength :- (s/maybe s/Num)
  [velocity  :- s/Num
   frequency :- s/Num]
  (when (not (zero? frequency))
    (/ velocity frequency)))

(s/defn velocity+wavelength->frequency :- (s/maybe s/Num)
  [velocity   :- s/Num
   wavelength :- s/Num]
  (when (not (zero? wavelength))
    (/ velocity wavelength)))

(s/defn wavelength+frequency->velocity :- s/Num
  [wavelength :- s/Num
   frequency  :- s/Num]
  (* wavelength frequency))
