(ns audiotools.logic.audio.harmony
  (:require [schema.core :as s]))

(s/defn voicing
  ""
  [])


(def harmonic-structures {:I   #{1 3 5}
                          :ii  #{2 4 6}
                          :iii #{3 5 7}
                          :IV  #{4 6 1}
                          :V   #{5 7 2 4}
                          :vi  #{6 1 3}
                          :vii #{7 2 4}})


(s/defn harmonic-coral
  "this function returns a list of harmonic structures"
  [base-notes :- [:I :ii :iii :IV :V :vi :vii]]
  (let [elements (->> base-notes
                      (map #(% harmonic-structures)))]
    elements))

(harmonic-coral [:I :IV :V :I])
