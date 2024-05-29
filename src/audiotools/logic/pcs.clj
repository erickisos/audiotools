(ns audiotools.logic.pcs
  (:require [audiotools.logic.notes :as notes]
            [clojure.set :refer [map-invert]]
            [clojure.string :as string]
            [schema.core :as s]))

(def NOTE_VALUES {:0 0 :1 1 :2 2 :3 3 :4 4 :5 5 :6 6 :7 7 :8 8 :9 9 :A 10 :B 11})

(s/defschema PitchClassString
  (s/with-fn-validation s/Str
    (fn [pcs]
      (every? #(contains? NOTE_VALUES (keyword %)) (string/split pcs #"")))))

(s/defschema PitchClassSet
  (s/with-fn-validation [s/Num]
    (fn [pcs] (let [notes (set (vals NOTE_VALUES))]
                (every? #(contains? notes %) pcs)))))

(s/defn str->pcs
  "Convert a string representation of the form \"134A\" to a list of pitch classes"
  [pcs-string :- PitchClassString]
  (map #(NOTE_VALUES (keyword %)) (string/split pcs-string #"")))

(s/defn pcs->str
  "Convert a list of pitch to the string representation"
  [pcs :- PitchClassSet]
  (->> pcs
       (map #(name (get (map-invert NOTE_VALUES) %)))
       (apply str)))

(s/defn permutate
  "calculate all the cyclic permutations of a pitch class set"
  [pcs :- PitchClassSet]
  (map #(take (count pcs) (drop % (cycle pcs))) (range (count pcs))))

(s/defn distance
  "calculate the distance between two notes"
  [a :- s/Num
   b :- s/Num]
  (mod (- b a) 12))

(s/defn distances
  "calculate the distances between all pairs of pitch classes in a set"
  [pcs :- PitchClassSet]
  (map (fn [[a b]] (distance a b)) (permutate pcs)))

(s/defn normal-form
  "calculate the basic form of a pitch class set"
  [pcs :- PitchClassSet]
  (let [permutations (-> pcs sort permutate)
        distances    (map (fn [d] [d (distance (first d) (last d))]) permutations)]
    (apply min-key (fn [[_ d]] d) distances)))

(s/defn inverse-form
  [pcs :- PitchClassSet]
  (map (fn [pc] (mod (- 12 pc) 12)) pcs))

(s/defn interval-pitch
  [note :- s/Num
   pcs :- PitchClassSet]
  (map #(distance note %) pcs))

(s/defn first-interval-pitch
  [pcs :- PitchClassSet]
  (interval-pitch (first pcs) (butlast (reverse pcs))))

;; To calculate the interval of first vs all
(comment "If you want to calculate the first vs all intervals"
         (let [pcs        '(10 11 1 3 4)
               first-note (first pcs)
               rest       (butlast (reverse pcs))]
           (interval-pitch first-note rest)))

(s/defn simplify
  [pcs :- PitchClassSet]
  (->> pcs
       (map #(notes/transpose-int % (- (first pcs))))
       (map #(if (<= % 6) % (mod % 6)))
       sort))

(s/defn compare-pcs
  "compare two pitch class sets stop returing the smaller one"
  [pcs1 :- PitchClassSet
   pcs2 :- PitchClassSet]
  (let [d1 (distances pcs1)
        d2 (distances pcs2)]
    (if (some #(> % 0) (map - d1 d2))
      pcs2
      pcs1)))

(s/defn interval-vector
  "Calculate the interval vector of a pitch class set"
  [pcs :- PitchClassSet]
  (let [d (distances pcs)]
    d))

(s/defn calculate
  [pcs :- PitchClassString]
  (let [bform               (-> pcs str->pcs normal-form first)
        intervals-bform (first-interval-pitch bform)
        iform               (inverse-form bform)
        basic-inverted-form (-> bform normal-form first)
        intervals-biform (first-interval-pitch basic-inverted-form)
        lower-intervals-form (compare-pcs intervals-bform intervals-biform)
        prime-form    (simplify lower-intervals-form)]
    {:normal-form                     bform
     :intervals-normal-form           intervals-bform
     :inverse-form                    iform
     :normal-inverted-form            basic-inverted-form
     :intervals-normal-inverted-form  intervals-biform
     :prime-form                      prime-form}))

(calculate "143BA")
(interval-vector '(10 11 1 3 4))

(simplify '(10 11 1 3 4))
(simplify '(2 1 11 9 8))

(compare-pcs '(10 11 1 3 4) '(2 1 11 9 8))
