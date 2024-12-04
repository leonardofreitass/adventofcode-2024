(ns adventofcode-2024.exercises.day-4.part-1
  (:require [clojure.string :as str]))

(def target ["X" "M" "A" "S"])

(def directions
  [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]])

(defn all-directions
  [pos]
  (for [direction directions]
    (for [i (range (count target))]
      [(+ (first pos) (* (first direction) i))
       (+ (second pos) (* (second direction) i))])))

(defn all-pos
  [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= (get-in grid [x y]) "X")]
    [x y]))

(defn get-many-in-grid
  [grid pos-arr]
  (mapv (fn [pos] (get-in grid pos)) pos-arr))

(defn is-valid?
  [grid pos-directions]
  (= (get-many-in-grid grid pos-directions) target))

(defn run
  [inputs]
  (let [grid (mapv #(str/split % #"") inputs)
        grid-pos (all-pos grid)
        grid-pos-directions (apply concat (map all-directions grid-pos))]
    (count (filter
            (partial is-valid? grid)
            grid-pos-directions))))
