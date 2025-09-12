(ns audiotools.logic.pcs.adapters
  (:require
   [audiotools.logic.pcs.types :refer [PitchClassSet PitchClassString]]
   [schema.core :as s]))

(s/defn pcstring->pcset :- PitchClassSet
  [pcs :- PitchClassString]
  (map #(case % \A 10 \B 11 (Integer/parseInt (str %))) pcs))

(s/defn pcset->pcstring :- PitchClassString
  [pcs :- PitchClassSet]
  (apply str (map #(case % 10 \A 11 \B %) pcs)))
