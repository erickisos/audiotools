(ns audiotools.logic.pcs
  (:require [schema.core :as s]))

(def NUMERICAL_NOTES {\1 1
                      \2 2
                      \3 3
                      \4 4
                      \5 5
                      \6 6
                      \7 7
                      \8 8
                      \9 9
                      \A 10
                      \B 11})

(s/defn pcs->notes
  [pcs :- s/Str]
  (map #(NUMERICAL_NOTES %) pcs))

(s/defn note->pcn
  [note :- s/Num]
  (first (map #(key %) (filter #(= note (val %)) NUMERICAL_NOTES))))

(s/defn distance
  [x-note :- s/Num
   y-note :- s/Num]
  (-> (- y-note x-note)
      (mod 12)))

(s/defn pcs->permutations
  [pcs :- s/Str]
  (->> (count pcs)
       range
       (map #(str (subs pcs % (count pcs)) (subs pcs 0 %)))))

(s/defn pcs->distance
  [pcs :- s/Str]
  (let [x-note (NUMERICAL_NOTES (first pcs))
        y-note (NUMERICAL_NOTES (last pcs))]
    (distance x-note y-note)))

(s/defn pcsets->distances-map
  [pcsets :- [s/Str]]
  (into {} (map (fn [pcs] [pcs (pcs->distance pcs)]) pcsets)))

(s/defn pcs->permutations-map
  [pcs :- s/Str]
  (->> (pcs->permutations pcs)
       (map (fn [permutation] [permutation (pcs->distance permutation)]))
       (into {})))

(s/defn pcs->ideal-form
  "The ideal form is the permutation with the smallest distance"
  [pcs :- s/Str]
  (let [permutations (pcsets->distances-map (pcs->permutations pcs))]
    (->> permutations
         (sort-by val)
         first
         key)))

(defn distances->ideal-form
  [distances]
  (->> distances (sort-by val) first key))

(s/defn note->inversion
  [note]
  (abs (- 12 (mod note 12))))

(s/defn pcs->inversion
  "Inverts every note in the pitch class set to their inversion"
  [pcs :- s/Str]
  (->> pcs
       pcs->notes
       (map note->inversion)
       (map note->pcn)
       (apply str)))

(s/defn first-to-rest-combinations
  "Calculate a map of the first character to the rest of the characters in a string starting from the end."
  [pcs :- s/Str]
  (let [first-char (first pcs)
        rest-chars (reverse (rest (seq pcs)))]
    (map #(str first-char %) rest-chars)))

(s/defn pcs->internal-distances :- s/Str
  [pcs :- s/Str]
  (->> pcs
       first-to-rest-combinations
       pcsets->distances-map
       vals
       (map str)
       (apply str)))

(s/defn pcs->basic-form
  [pcs :- s/Str]
  (let [ideal-form          (pcs->ideal-form pcs)
        inverted-ideal-form (-> ideal-form
                                pcs->inversion
                                pcs->ideal-form)
        iif-distances       (pcs->internal-distances inverted-ideal-form)
        if-distances        (pcs->internal-distances ideal-form)]
    (if (compare iif-distances if-distances)
      inverted-ideal-form
      ideal-form)))

(defn basic-form
  [ideal-form inverted-ideal-form]
  (let [iif-distances (pcs->internal-distances inverted-ideal-form)
        if-distances  (pcs->internal-distances ideal-form)]
    (if (compare if-distances iif-distances)
      ideal-form
      inverted-ideal-form)))

(s/defn transpose
  [note :- s/Num
   t-factor :- s/Num]
  (mod (+ note t-factor) 12))

(s/defn prime-form
  [basic-form :- s/Str]
  (let [notes    (pcs->notes basic-form)
        t-factor (- 12 (first notes))]
    (map #(transpose % t-factor) notes)))

(s/defn calculate-forms
  [pitch-class-set :- s/Str]
  (let [distances              (pcsets->distances-map (pcs->permutations pitch-class-set))
        ideal-form             (distances->ideal-form distances)
        inverted-ideal-form    (pcs->inversion ideal-form)
        ;; TODO: Check why we need to reverse the inverted ideal form
        reversed-iif           (apply str (reverse (seq inverted-ideal-form)))
        reversed-iif-distances (pcsets->distances-map (pcs->permutations reversed-iif))
        rideal-iif             (distances->ideal-form reversed-iif-distances)
        basic-form             (basic-form ideal-form rideal-iif)
        t-factor               (- 12 (first (pcs->notes basic-form)))
        prime-form             (prime-form basic-form)]
    (println "=== Pitch Class Set ===")
    (println pitch-class-set)
    (println "Distances: " distances)
    (println "Ideal form: " ideal-form)
    (println "Inverted ideal form: " inverted-ideal-form)
    (println "RIIF Distances: " reversed-iif-distances)
    (println "Ideal form of inverted ideal form: " rideal-iif)
    (println "Basic form: " basic-form)
    (println "Prime form T(" t-factor "): " prime-form)))
