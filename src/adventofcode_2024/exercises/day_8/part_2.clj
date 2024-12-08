(ns adventofcode-2024.exercises.day-8.part-2
  (:require [clojure.string :as str]))


(defn generate-antenna-pos-map
  [grid]
  (reduce
   (fn [acc [key & value]]
     (update acc key #(conj % value)))
   {}
   (for [y (range (count grid))
         x (range (count (first grid)))
         :let [ch (get-in grid [y x])]
         :when (not= ch ".")]
     [ch y x])))

(defn pos-diff
  [[a-y a-x] [b-y b-x]]
  [(- a-y b-y) (- a-x b-x)])

(defn not-out-of-bounds?
  [max-y max-x [y x]]
  (not (or (< y 0) (< x 0) (>= y max-y) (>= x max-x))))

(defn iterate-antinodes
  [max-y max-x antenna diff]
  (loop [pos antenna
         antinodes [antenna]]
    (let [new-pos (map + pos diff)]
      (if (not-out-of-bounds? max-y max-x new-pos)
        (recur new-pos (conj antinodes new-pos))
        antinodes))))

(defn generate-antinodes
  [antenna-pos max-x max-y]
  (filter
   (partial not-out-of-bounds? max-y max-x)
   (apply concat (for [a antenna-pos
                  b antenna-pos
                  :when (not= a b)]
              (iterate-antinodes max-y max-x a (pos-diff a b))))))

(defn run
  [inputs]
  (let [grid (mapv #(str/split % #"") inputs)
        max-y (count grid)
        max-x (count (first grid))
        antenna-pos-map (generate-antenna-pos-map grid)]
    (count (reduce
            (fn [s antenna-pos]
              (apply conj s (generate-antinodes antenna-pos max-x max-y)))
            #{}
            (vals antenna-pos-map)))))