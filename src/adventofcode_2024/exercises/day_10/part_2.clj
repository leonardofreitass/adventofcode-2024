(ns adventofcode-2024.exercises.day-10.part-2
  (:require [clojure.string :as str]))

(def trail-start 0)
(def trail-end 9)

(def adjacent-adjustments
  [[0 -1] [-1  0] [1  0] [0  1]])

(defn parse-inputs
  [inputs]
  (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line #""))) inputs))

(defn find-trailheads
  [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= (get-in grid [x y]) trail-start)]
    [x y]))

(defn trailhead-scores
  [grid pos height]
  (let [adjacent (map #(map + %1 %2) adjacent-adjustments (repeat pos))
        slopes (filter #(= (inc height) (get-in grid %)) adjacent)]
    (cond
      (= height (dec trail-end)) (count slopes)
      (empty? slopes) 0
      :else (reduce + (map #(trailhead-scores grid % (inc height)) slopes)))))

(defn run
  [inputs]
  (let [grid (parse-inputs inputs)
        trailheads (find-trailheads grid)]
    (reduce + (map #(trailhead-scores grid % trail-start) trailheads))))
