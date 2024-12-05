(ns adventofcode-2024.exercises.day-5.part-2
  (:require [clojure.string :as str]))

(defn index-of [lst elem]
  (or (some (fn [[i v]] (when (= v elem) i))
            (map-indexed vector lst))
      -1))

(defn map-rules
  [rules]
  (reduce
   (fn [acc [value key]]
     (update acc key #(conj % value)))
   {}
   rules))

(defn map-inverse-rules
  [rules]
  (reduce
   (fn [acc [key value]]
     (update acc key #(conj % value)))
   {}
   rules))

(defn invalid?
  [rules update-arr]
  (let [update-set (set update-arr)]
    (loop [n (first update-arr)
           p #{}
           s (rest update-arr)]
      (if (nil? n)
        false
        (let [r (get rules n)]
          (if (some #(and (contains? update-set %) (not (contains? p %))) r)
            true
            (recur (first s) (conj p n) (rest s))))))))

(defn correct-invalid
  [rules update-arr]
  (loop [arr (reverse update-arr)
         processed '()]
    (if (empty? arr)
      (vec processed)
      (let [n (first arr)
            r (get rules n)
            i (if (nil? r) -1 (apply max (map (partial index-of arr) r)))]
        (recur
         (if (= i -1) (next arr) (concat (take i (next arr)) [n] (drop (inc i) arr)))
         (if (= i -1) (conj processed n) processed))))))

(defn run
  [inputs]
  (let [[_rules _updates] (split-with (partial not= "") inputs)
        rules (map #(str/split % #"\|") _rules)
        updates (map #(str/split % #",") (next _updates))
        invalid (filter
                   (partial invalid? (map-rules rules))
                   updates)]
    (reduce
     (fn [acc corrected-updates] 
       (+ acc (Integer/parseInt (get corrected-updates (/ (dec (count corrected-updates)) 2)))))
     0
     (map (partial correct-invalid (map-inverse-rules rules)) invalid))))
