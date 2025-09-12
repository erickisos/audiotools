(ns audiotools.logic.pcs
  (:require
   [audiotools.logic.pcs.adapters :refer [pcstring->pcset]]
   [audiotools.logic.pcs.operations :refer [compare-sets idealize invert
                                            normalize simplify]]
   [audiotools.logic.pcs.types :refer [PitchClassString]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [schema.core :as s]))

#_(defmacro tapd
    [expr message]
    `(let [x# ~expr]
       (println (or ~message '~expr) "\n\t" x#)
       x#))

(s/defn calculate
  [pcs :- PitchClassString]
  (let [original                 (-> pcs pcstring->pcset sort idealize)
        original-ideal-invertido (-> original :ideal invert)
        invertido                (-> original-ideal-invertido sort idealize)
        original-basica          (-> original :ideal normalize)
        invertida-basica         (-> invertido :ideal normalize)
        forma-basica             (compare-sets (:ideal original) (:ideal invertido))]
    {:original         (assoc original :ideal-invertido original-ideal-invertido
                              :basico original-basica)
     :invertido        (assoc invertido :basico invertida-basica)
     :original-basica  original-basica
     :invertida-basica invertida-basica
     :forma-basica     forma-basica
     :forma-primaria   (simplify forma-basica)}))

(s/defn calculate-ideal-steps
  [pcs :- PitchClassString]
  (let [sorted-pcs                       (-> pcs pcstring->pcset sort)
        {:keys [ideal sorted-rotations]} (idealize sorted-pcs)
        rotations-message                (map (fn [[p distance]] (format "    - $(%s) = %d$" (str/join "," p) distance)) sorted-rotations)]
    (println "## Pitch Class Set: " pcs)
    (println (format "1. Ordenando el set: $(%s)$" (str/join "," sorted-pcs)))
    (println "2. Calculando las permutaciones de la forma ordenada:")
    (run! println rotations-message)
    (println "3. Forma Ideal: " ideal)))

(s/defn calculate-basic-and-prime-steps
  [pcs :- PitchClassString]
  (let [{:keys [original-ideal
                original-ideal-invertido
                invertido-ideal
                original-basica
                invertida-basica
                forma-basica
                forma-primaria]} (calculate pcs)]
    (println "## Pitch Class Set: " pcs)
    (println "1. Forma ideal: " original-ideal)
    (println "2. Forma Ideal Invertida: " original-ideal-invertido)
    (println "3. Forma Invertida Ideal: " invertido-ideal)
    (println "4. Forma Básica del Original: " original-basica)
    (println "5. Forma Básica del Invertido: " invertida-basica)
    (println "6. Forma Básica: " forma-basica)
    (println "7. Forma Primaria: " forma-primaria)))

(s/defn calculate-forms
  [pcs :- PitchClassString]
  (let [{:keys [original-ideal
                original-ideal-invertido
                invertido-ideal
                original-basica
                invertida-basica
                forma-basica
                forma-primaria]} (calculate pcs)]
    (println "=== Calculando para " pcs "===")
    (println "Forma original ideal: " original-ideal)
    (println "Forma original ideal invertida: " original-ideal-invertido)
    (println "Forma invertida ideal: " invertido-ideal)
    (println "Forma básica original: " original-basica)
    (println "Forma básica invertida: " invertida-basica)
    (println "Forma básica: " forma-basica)
    (println "Forma primaria: " forma-primaria)))

(calculate "143BA")
(def class-sets ["0482" "3621" "9357A" "14302" "4608B"])
(calculate-ideal-steps "143BA")
(run! calculate-ideal-steps class-sets)
(run! calculate-basic-and-prime-steps class-sets)

(pprint (calculate "143BA"))
