(ns adventofcode-2024.exercises.day-11.part-1 
  (:require [clojure.string :as str]))

(def iterations 25)

(defn next-number
  [n]
  (let [str-n (str n)
        digits (count str-n)]
    (cond
      (zero? n) [1]
      (even? digits) [(Integer/parseInt (subs str-n 0 (/ digits 2)))
                      (Integer/parseInt (subs str-n (/ digits 2)))]
      :else [(* 2024 n)])))

(def memoized-next-number
  (memoize next-number))

(defn parse-inputs
  [inputs]
  (mapv #(Integer/parseInt %) (str/split (first inputs) #" ")))

(defn run
  [inputs]
  (count (reduce
          (fn [numbers _]
            (mapcat memoized-next-number numbers))
          (parse-inputs inputs)
          (range iterations))))
