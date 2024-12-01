(ns adventofcode-2024.exercises.day-1.part-2
  (:require [clojure.string :as str]))

(defn count-occurrences [s slist]
  (->> slist
       flatten
       (filter #{s})
       count))

(defn process-input
  [inputs]
  (reduce
   (fn [[left right] line]
     (let [[a b] (str/split line #"   ")]
       [(conj left (Integer/parseInt a)) (conj right (Integer/parseInt b))]))
   [[] []]
   inputs))

(defn run
  [inputs]
  (let [[left right] (process-input inputs)]
    (reduce
     (fn [acc a]
       (+ acc (* a (count-occurrences a right))))
     0
     left)))
