(ns adventofcode-2024.exercises.day-3.part-2
  (:require [clojure.string :as str]))

(defn run
  [inputs]
  (reduce
   (fn [acc [_ a b]] (+ acc (* (Integer/parseInt a) (Integer/parseInt b))))
   0
   (re-seq
    #"mul\((\d+),(\d+)\)"
    (str/replace (first inputs) #"don't\(\)(.*?)(?=do\(\))" ""))))
