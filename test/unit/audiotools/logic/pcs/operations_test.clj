(ns unit.audiotools.logic.pcs.operations-test
  (:require
   [audiotools.logic.pcs.operations :as pcs.operations]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(deftest transpose-pitch
  (testing "it can transpose any pitch"
    (is (= 1 (pcs.operations/transpose-pitch 0 1)))
    (is (= 0 (pcs.operations/transpose-pitch 11 1))))
  (testing "it can transpose any pitch by a negative factor"
    (is (= 11 (pcs.operations/transpose-pitch 0 -1)))
    (is (= 0 (pcs.operations/transpose-pitch 1 -1))))
  (testing "it will normalize any transposition that goes over the limit"
    (is (= 10 (pcs.operations/transpose-pitch 11 35)))))

(deftest trasnspose
  (testing "it can transpose a simple PitchClassSet"
    (is (= '(0 3 5) (pcs.operations/transpose '(3 6 8) -3)))))

(deftest transposition-factor
  (testing "it can calculate the transposition factor for a simple PitchClassSet"
    (is (= 0 (pcs.operations/transposition-factor '(0 3 5))))
    (is (= -1 (pcs.operations/transposition-factor '(1 4 6)))))
  (testing "it calculate the transposition factor with the shortest path to 0"
    (is (= 6 (pcs.operations/transposition-factor '(6 9 11))))
    (is (= 5 (pcs.operations/transposition-factor '(7 10 0))))
    (is (= 4 (pcs.operations/transposition-factor '(8 10 1))))))

(def data [{:pcs              "0482"
            :original         {:rotations        '('(0 2 4 8) '(2 4 8 0) '(4 8 0 2) '(8 0 2 4)),
                               :sorted-rotations '(['(0 2 4 8) 8] ['(8 0 2 4) 8] ['(2 4 8 0) 10] ['(4 8 0 2) 10]),
                               :ideal            '(0 2 4 8),
                               :ideal-invertido  '(0 10 8 4),
                               :basico           '(8 4 2)},
            :invertido        {:rotations        '('(0 4 8 10) '(4 8 10 0) '(8 10 0 4) '(10 0 4 8)),
                               :sorted-rotations '(['(4 8 10 0) 8] ['(8 10 0 4) 8] ['(0 4 8 10) 10] ['(10 0 4 8) 10]),
                               :ideal            '(4 8 10 0),
                               :basico           '(8 6 4)},
            :original-basica  '(8 4 2),
            :invertida-basica '(8 6 4),
            :forma-basica     '(0 2 4 8),
            :forma-primaria   '(0 2 4 8)}
           {:pcs              "3621"
            :original         {:rotations        '('(1 2 3 6) '(2 3 6 1) '(3 6 1 2) '(6 1 2 3)),
                               :sorted-rotations '(['(1 2 3 6) 5] ['(6 1 2 3) 9] ['(2 3 6 1) 11] ['(3 6 1 2) 11]),
                               :ideal            '(1 2 3 6),
                               :ideal-invertido  '(11 10 9 6),
                               :basico           '(5 2 1)},
            :invertido        {:rotations        '('(6 9 10 11) '(9 10 11 6) '(10 11 6 9) '(11 6 9 10)),
                               :sorted-rotations '(['(6 9 10 11) 5] ['(9 10 11 6) 9] ['(10 11 6 9) 11] ['(11 6 9 10) 11]),
                               :ideal            '(6 9 10 11),
                               :basico           '(5 4 3)},
            :original-basica  '(5 2 1),
            :invertida-basica '(5 4 3),
            :forma-basica     '(1 2 3 6),
            :forma-primaria   '(0 1 2 5)}
           {:pcs              "9357A"
            :original         {:rotations        '('(3 5 7 9 10) '(5 7 9 10 3) '(7 9 10 3 5) '(9 10 3 5 7) '(10 3 5 7 9)),
                               :sorted-rotations '(['(3 5 7 9 10) 7]
                                                   ['(5 7 9 10 3) 10]
                                                   ['(7 9 10 3 5) 10]
                                                   ['(9 10 3 5 7) 10]
                                                   ['(10 3 5 7 9) 11]),
                               :ideal            '(3 5 7 9 10),
                               :ideal-invertido  '(9 7 5 3 2),
                               :basico           '(7 6 4 2)},
            :invertido        {:rotations        '('(2 3 5 7 9) '(3 5 7 9 2) '(5 7 9 2 3) '(7 9 2 3 5) '(9 2 3 5 7)),
                               :sorted-rotations '(['(2 3 5 7 9) 7]
                                                   ['(5 7 9 2 3) 10]
                                                   ['(7 9 2 3 5) 10]
                                                   ['(9 2 3 5 7) 10]
                                                   ['(3 5 7 9 2) 11]),
                               :ideal            '(2 3 5 7 9),
                               :basico           '(7 5 3 1)},
            :original-basica  '(7 6 4 2),
            :invertida-basica '(7 5 3 1),
            :forma-basica     '(2 3 5 7 9),
            :forma-primaria   '(0 1 3 5 7)}
           {:pcs              "14302"
            :original         {:rotations        '('(0 1 2 3 4) '(1 2 3 4 0) '(2 3 4 0 1) '(3 4 0 1 2) '(4 0 1 2 3)),
                               :sorted-rotations '(['(0 1 2 3 4) 4]
                                                   ['(1 2 3 4 0) 11]
                                                   ['(2 3 4 0 1) 11]
                                                   ['(3 4 0 1 2) 11]
                                                   ['(4 0 1 2 3) 11]),
                               :ideal            '(0 1 2 3 4),
                               :ideal-invertido  '(0 11 10 9 8),
                               :basico           '(4 3 2 1)},
            :invertido        {:rotations        '('(0 8 9 10 11)
                                                   '(8 9 10 11 0)
                                                   '(9 10 11 0 8)
                                                   '(10 11 0 8 9)
                                                   '(11 0 8 9 10)),
                               :sorted-rotations '(['(8 9 10 11 0) 4]
                                                   ['(0 8 9 10 11) 11]
                                                   ['(9 10 11 0 8) 11]
                                                   ['(10 11 0 8 9) 11]
                                                   ['(11 0 8 9 10) 11]),
                               :ideal            '(8 9 10 11 0),
                               :basico           '(4 3 2 1)},
            :original-basica  '(4 3 2 1),
            :invertida-basica '(4 3 2 1),
            :forma-basica     '(0 1 2 3 4),
            :forma-primaria   '(0 1 2 3 4)}
           {:pcs              "4608B"
            :original         {:rotations        '('(0 4 6 8 11) '(4 6 8 11 0) '(6 8 11 0 4) '(8 11 0 4 6) '(11 0 4 6 8)),
                               :sorted-rotations '(['(4 6 8 11 0) 8]
                                                   ['(11 0 4 6 8) 9]
                                                   ['(6 8 11 0 4) 10]
                                                   ['(8 11 0 4 6) 10]
                                                   ['(0 4 6 8 11) 11]),
                               :ideal            '(4 6 8 11 0),
                               :ideal-invertido  '(8 6 4 1 0),
                               :basico           '(8 7 4 2)},
            :invertido        {:rotations        '('(0 1 4 6 8) '(1 4 6 8 0) '(4 6 8 0 1) '(6 8 0 1 4) '(8 0 1 4 6)),
                               :sorted-rotations '(['(0 1 4 6 8) 8]
                                                   ['(4 6 8 0 1) 9]
                                                   ['(6 8 0 1 4) 10]
                                                   ['(8 0 1 4 6) 10]
                                                   ['(1 4 6 8 0) 11]),
                               :ideal            '(0 1 4 6 8),
                               :basico           '(8 6 4 1)},
            :original-basica  '(8 7 4 2),
            :invertida-basica '(8 6 4 1),
            :forma-basica     '(0 1 4 6 8),
            :forma-primaria   '(0 1 4 6 8)}])

(defn print-process
  [{:keys [pcs original invertido forma-basica forma-primaria]}]
  (println "## Pitch Class Set: " pcs)
  (println "1. Obtenemos las rotaciones de la forma ordenada")
  (run! (fn [[p distance]] (println (format "    - $(%s) = %d$" (str/join "," (second p)) distance))) (:sorted-rotations original))
  (println (format "2. Dadas las rotaciones, la forma ideal es: $(%s)$" (str/join "," (:ideal original))))
  (println (format "3. La forma básica a partir de dicho ideal es: $(%s)$" (str/join "," (:basico original))))
  (println (format "4. Ahora, el inverso de la forma ideal original es: $(%s)$" (str/join "," (:ideal-invertido original))))
  (println "5. A partir del inverso, calculamos las rotaciones")
  (run! (fn [[p distance]] (println (format "    - $(%s) = %d$" (str/join "," (second p)) distance))) (:sorted-rotations invertido))
  (println (format "5. De lo anterior, concluimos que la forma ideal del invertido es: $(%s)$" (str/join "," (:ideal invertido))))
  (println (format "6. La forma básica del invertido es: $(%s)$" (str/join "," (:basico invertido))))
  (println (format "7. Ahora que tenemos ambas formas básicas, podemos determinar que la forma básica global es: $(%s)$" (str/join "," forma-basica)))
  (println (format "8. Finalmente, la forma primaria es: $(%s)$" (str/join "," forma-primaria))))

(run! print-process data)
