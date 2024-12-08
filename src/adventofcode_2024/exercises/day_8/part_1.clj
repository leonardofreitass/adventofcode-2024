(ns adventofcode-2024.exercises.day-8.part-1
  (:require [clojure.string :as str]))

(defn generate-antena-pos-map
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

(defn generate-antinodes
  [antena-pos max-x max-y]
  (filter
   (partial not-out-of-bounds? max-y max-x)
   (for [a antena-pos
         b antena-pos
         :when (not= a b)]
     (map + a (pos-diff a b)))))

(defn run
  [inputs]
  (let [grid (mapv #(str/split % #"") inputs)
        max-y (count grid)
        max-x (count (first grid))
        antena-pos-map (generate-antena-pos-map grid)]
    (count (reduce
            (fn [s antena-pos]
              (apply conj s (generate-antinodes antena-pos max-x max-y)))
            #{}
            (vals antena-pos-map)))))
