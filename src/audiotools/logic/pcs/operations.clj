(ns audiotools.logic.pcs.operations
  (:require
   [audiotools.logic.pcs.types :refer [Pitch PitchClassSet]]
   [schema.core :as s]))

(s/defn transpose-pitch :- Pitch
  [pitch :- Pitch
   factor :- s/Num]
  (mod (+ pitch factor) 12))

(s/defn transpose :- PitchClassSet
  [pcs :- PitchClassSet
   factor :- s/Num]
  (map #(transpose-pitch % factor) pcs))

(s/defn rotate :- PitchClassSet
  [pcs :- PitchClassSet
   n   :- s/Num]
  (let [n (mod n (count pcs))]
    (concat (drop n pcs) (take n pcs))))

(s/defn distance :- s/Num
  [pcs :- PitchClassSet]
  (mod (- (last pcs) (first pcs)) 12))

(s/defn transposition-factor
  [[first-pitch] :- PitchClassSet]
  (let [inc-factor (- 12 first-pitch)
        dec-factor (- first-pitch)]
    (if (> (abs inc-factor) (abs dec-factor))
      dec-factor
      inc-factor)))

(s/defn idealize
  [pcs :- PitchClassSet]
  (let [rotations        (map #(rotate pcs %) (range (count pcs)))
        add-distance-fn  (fn [p] [p (distance p)])
        sorted-rotations (->> rotations
                              (map add-distance-fn)
                              (sort-by second))]
    {:rotations        rotations
     :sorted-rotations sorted-rotations
     :ideal            (first (first sorted-rotations))}))

(s/defn invert-note
  [n :- s/Num]
  (mod (- 12 n) 12))

(s/defn invert
  [pcs :- PitchClassSet]
  (map invert-note pcs))

(s/defn normalize
  [pcs :- PitchClassSet]
  (->> pcs
       rest
       reverse
       (map #(list (first pcs) %))
       (map distance)))

(s/defn compare-sets :- PitchClassSet
  [pcs1 :- PitchClassSet
   pcs2 :- PitchClassSet]
  (let [zipped (map vector (normalize pcs1) (normalize pcs2))]
    (loop [sets zipped]
      (if-let [[a b] (first sets)]
        (cond
          (< a b) pcs1
          (> a b) pcs2
          :else (recur (rest sets)))
        pcs1))))

(s/defn simplify
  [pcs :- PitchClassSet]
  (->> pcs
       transposition-factor
       (transpose pcs)
       #_(map (fn [n] (cond-> n (> n 6) invert-note)))))
