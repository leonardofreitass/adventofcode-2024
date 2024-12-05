(ns adventofcode-2024.exercises.day-5.part-1
  (:require [clojure.string :as str]))

(defn map-rules
  [rules]
  (reduce
   (fn [acc [value key]]
     (update acc key #(conj % value)))
   {}
   rules))

(defn valid?
  [rules update-arr]
  (let [update-set (set update-arr)]
    (loop [n (first update-arr)
           p #{}
           s (rest update-arr)]
      (if (nil? n)
        true
        (let [r (get rules n)]
          (if (some #(and (contains? update-set %) (not (contains? p %))) r)
            false
            (recur (first s) (conj p n) (rest s))))))))

(defn run
  [inputs]
  (let [[_rules _updates] (split-with (partial not= "") inputs)
        rules (map #(str/split % #"\|") _rules)
        rules-map (map-rules rules)
        updates (map #(str/split % #",") (next _updates))]
    (reduce 
     #(+ %1 (Integer/parseInt (get %2 (/ (dec (count %2)) 2))))
     0
     (filter
      (partial valid? rules-map)
      updates))))
