(ns adventofcode-2024.exercises.day-4.part-1
  (:require [clojure.string :as str]))

(def target "XMAS")

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
        x (range (count (first grid)))]
    [x y]))

(defn get-many-in-grid
  [grid pos-arr]
  (str/join "" (mapv (fn [pos] (get-in grid pos)) pos-arr)))

(defn run
  [inputs]
  (let [grid (mapv #(str/split % #"") inputs)
        grid-pos (all-pos grid)
        grid-pos-directions (apply concat (mapv all-directions grid-pos))]
    (count (filter
            (fn [pos-directions]
              (= (get-many-in-grid grid pos-directions) target))
            grid-pos-directions))))
