(ns adventofcode-2024.exercises.day-10.part-1
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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
      (= height (dec trail-end)) (set slopes)
      (empty? slopes) #{}
      :else (apply set/union (map #(trailhead-scores grid % (inc height)) slopes)))))

(defn run
  [inputs]
  (let [grid (parse-inputs inputs)
        trailheads (find-trailheads grid)]
    (count (apply concat (map
                   #(trailhead-scores grid % trail-start)
                   trailheads)))))
