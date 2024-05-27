(ns audiotools.logic.pcs
  (:require [schema.core :as s]))

(def NUMERICAL_NOTES {\0 0
                      \1 1
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

(defn distances->ideal-form
  [distances]
  (->> distances (sort-by val) first key))

(s/defn note->inversion
  [note]
  (let [value (abs (- 12 (mod note 12)))]
    (if (= value 12)
      0
      value)))

(s/defn notes->pcs
  [notes :- [s/Num]]
  (apply str (map note->pcn notes)))

(s/defn pcs->inversion
  "Inverts every note in the pitch class set to their inversion"
  [pcs :- s/Str]
  (->> pcs
       pcs->notes
       (map note->inversion)
       notes->pcs))

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

(defn basic-form
  [ideal-form inverted-ideal-form]
  (let [iif-distances (pcs->internal-distances inverted-ideal-form)
        if-distances  (pcs->internal-distances ideal-form)]
    (if (compare if-distances iif-distances)
      ideal-form
      inverted-ideal-form)))

(s/defn transpose
  [note :- s/Num
   transposition-number :- s/Num]
  (mod (+ note transposition-number) 12))

(s/defn transposition-number
  [notes :- [s/Num]]
  (let [inc-factor (- 12 (first notes))
        dec-factor (- (first notes))]
    (if (> (abs inc-factor) (abs dec-factor))
      dec-factor
      inc-factor)))

(s/defn calculate-forms
  [pitch-class-set :- s/Str]
  (let [permutations              (pcs->permutations pitch-class-set)
        distances                 (pcsets->distances-map permutations)
        ideal-form                (distances->ideal-form distances)
        inverted-ideal-form       (pcs->inversion ideal-form)
        ;; TODO: Check why we need to reverse the inverted ideal form
        reversed-iif              (apply str (reverse (seq inverted-ideal-form)))
        reversed-iif-permutations (pcs->permutations reversed-iif)
        reversed-iif-distances    (pcsets->distances-map reversed-iif-permutations)
        rideal-iif                (distances->ideal-form reversed-iif-distances)
        basic-form                (basic-form ideal-form rideal-iif)
        bf-notes                  (pcs->notes basic-form)
        transposition-number                  (transposition-number bf-notes)
        transposed-notes          (map #(transpose % transposition-number) bf-notes)
        transposed-form           (notes->pcs transposed-notes)
        prime-form                (sort transposed-notes)]
    (println "## PCS: " pitch-class-set)
    (println "1. **Permutaciones del Set**: " permutations)
    (println "2. **Distancias por permutación**: ")
    (doseq [[k v] distances]
      (println "    1. " k " -> " v))
    (println "3. **Forma Ideal**: " ideal-form)
    (println "4. **Invertido**: " inverted-ideal-form)
    (println "5. **FII**: " rideal-iif)
    (println "6. **Forma Basica**: " basic-form)
    (println "7. **Transposición**: $T(" transposition-number ") = " transposed-form "$")
    (println "8. **Forma Primaria**: " prime-form)))

;; (def class-sets ["0482" "3621" "9357A" "14302" "4608B"])
;; (run! calculate-forms class-sets)

(calculate-forms "0325")

;; Antes de rotar el número hay que ordenarlo.
;; El conjunto complementario es la lista de todos los elementos que no están siendo usados en el PCS Original.
;; TODO: Normalizar los semitonos, cualquier cosa por encima de 6 semitonos es su inverso (Para 7 semitonos, el número que podemos usar es 5).
