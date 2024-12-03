(ns adventofcode-2024.exercises.day-3.part-1)

(defn run
  [inputs]
  (reduce
   (fn [acc [_ a b]] (+ acc (* (Integer/parseInt a) (Integer/parseInt b))))
   0
   (re-seq #"mul\((\d+),(\d+)\)" (first inputs))))
