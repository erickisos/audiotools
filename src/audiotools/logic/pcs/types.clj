(ns audiotools.logic.pcs.types
  (:require
   [schema.core :as s]))

(s/defschema Pitch (s/with-fn-validation s/Num #(contains? #{0 1 2 3 4 5 6 7 8 9 10 11} %)))
(s/defschema PitchClassString
  (s/with-fn-validation s/Str (fn [pcs] (re-matches #"[0-9A-B]+" pcs))))
(s/defschema PitchClassSet [Pitch])
