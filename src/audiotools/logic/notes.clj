(ns audiotools.logic.notes
  (:require [schema.core :as s]))

(def NOTES {:C  0
            :C# 1
            :Db 1
            :D  2
            :D# 3
            :Eb 3
            :E  4
            :F  5
            :F# 6
            :Gb 6
            :G  7
            :G# 8
            :Ab 8
            :A  9
            :A# 10
            :Bb 10
            :B  11})

(s/defn note->int
  [note :- #{:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B}]
  (NOTES note))

(s/defn int->note
  [int :- s/Num]
  (some (fn [[k v]] (when (= v int) k)) NOTES))

(s/defn transpose-int
  [int :- s/Num
   steps :- s/Num]
  (mod (+ int steps) 12))

(s/defn transpose-note
  [note :- #{:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B}
   semitones :- s/Num]
  (-> note
      note->int
      (transpose-int semitones)
      int->note))
