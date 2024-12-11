(ns adventofcode-2024.exercises.day-11.part-2
  (:require [clojure.string :as str]))

(def iterations 75)

(defn count-uniques
  [list]
  (reduce
   (fn [acc n] (update-in acc [n] (fnil inc 0)))
   {}
   list))

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
  (count-uniques (mapv #(Integer/parseInt %) (str/split (first inputs) #" "))))

(defn run
  [inputs]
  (reduce + (vals (reduce
                   (fn [n-map _]
                     (reduce-kv
                      (fn [acc n x]
                        (reduce
                         #(update-in %1 [%2] (fnil + 0) x)
                         acc
                         (memoized-next-number n)))
                      {}
                      n-map))
                   (parse-inputs inputs)
                   (range iterations)))))
