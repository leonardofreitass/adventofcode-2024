(ns adventofcode-2024.exercises.day-14.part-2 
  (:require
    [clojure.string :as str]))

(def mod-arr [101 103])
(def target "###############################")

(def empty-grid
  (vec (repeat (second mod-arr) (vec (repeat (first mod-arr) " ")))))

(defn grid-with-robots
  [robots]
  (map
   (partial str/join "")
   (reduce
    (fn [grid [[x y]]]
      (assoc-in grid [y x] "#"))
    empty-grid
    robots)))

(defn parse-inputs
  [inputs]
  (map (fn [input] (partition 2 (mapv #(Integer/parseInt %) (re-seq #"-?\d+" input)))) inputs))

(defn pass-second
  [robots]
  (map
   (fn [[initial speed]]
     [(map
       #(mod (+ %2 %3) %1)
       mod-arr initial speed)
      speed])
   robots))

(defn run
  [inputs]
  (loop [i 0
         robots (parse-inputs inputs)]
    (if (some #(str/includes? % target) (grid-with-robots robots))
      i
      (recur (inc i) (pass-second robots)))))
 