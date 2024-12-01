(ns adventofcode-2024.exercises.day-1.part-1
  (:require [clojure.string :as str]))

(defn process-input
  [inputs]
  (->> inputs
       (reduce
        (fn [[left right] line]
          (let [[a b] (str/split line #"   ")]
            [(conj left (Integer/parseInt a))
             (conj right (Integer/parseInt b))]))
        [[] []])
       (mapv sort)))

(defn run
  [inputs]
  (reduce
   (fn [acc [left right]]
     (+ acc (abs (- left right))))
   0
   (apply map vector (process-input inputs))))
