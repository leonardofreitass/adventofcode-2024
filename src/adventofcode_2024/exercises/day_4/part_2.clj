(ns adventofcode-2024.exercises.day-4.part-2
  (:require [clojure.string :as str]))

(def target "MAS")
(def reverse-target (str/reverse target))

(def directions
  [[-1 -1] [-1 1]])

(defn all-directions
  [pos]
  (for [direction directions]
    (let [start (mapv + pos direction)]
      (for [i (range (count target))]
        [(+ (first start) (* (first direction) i -1))
         (+ (second start) (* (second direction) i -1))]))))

(defn all-pos
  [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= (get-in grid [x y]) "A")]
    [x y]))

(defn get-many-in-grid
  [grid pos-arr]
  (str/join "" (mapv (fn [pos] (get-in grid pos)) pos-arr)))

(defn run
  [inputs]
  (let [grid (mapv #(str/split % #"") inputs)
        grid-pos (all-pos grid)
        grid-pos-directions (mapv all-directions grid-pos)]
    (count (filter
            (fn [words]
              (every? #(or (= target %) (= reverse-target %)) words))
            (map #(mapv (partial get-many-in-grid grid) %) grid-pos-directions)))))
