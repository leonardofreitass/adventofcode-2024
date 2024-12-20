(ns adventofcode-2024.exercises.day-20.part-2
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def directions [[-1 0] [0 1] [1 0] [0 -1]])
(def max-range 20)
(def cost-target 100)

(defn side-positions [pos cost]
  (map (fn [move] [(mapv + pos move) (inc cost)])
       directions))

(def unblocked-chs #{\. \S \E})

(defn find-coordinates [grid value]
  (first
   (for [x (range (count grid))
         y (range (count (nth grid x)))
         :let [cell (nth (nth grid x) y)]
         :when (= cell value)]
     [x y])))

(defn assoc-pos
  [m l]
  (reduce (fn [a [p c]] (assoc a p c)) m l))

(defn shortest-path
  [grid start]
  (loop [queue (priority-map start 0)
         costs {start 0}]
    (if (empty? queue)
      costs
      (let [[pos cost] (peek queue)
            queue (pop queue)
            next-positions (filter
                            (fn [[pos cost]]
                              (and (not= \# (get-in grid pos))
                                   (or (nil? (get costs pos))
                                       (< cost (get costs pos)))))
                            (side-positions pos cost))]
        (recur (assoc-pos queue next-positions) (assoc-pos costs next-positions))))))

(defn in-range-cells
  [grid [sx sy]]
  (set (for [dx (range (- max-range) (inc max-range))
             dy (range (- max-range) (inc max-range))
             :let [pos [(+ sx dx) (+ sy dy)]
                   distance (+ (Math/abs dx) (Math/abs dy))
                   cell (get-in grid pos)]
             :when (and (<= distance max-range)
                        (contains? unblocked-chs cell))]
         [pos distance])))

(defn unblocked-pos
  [grid]
  (for [x (range (count grid))
        y (range (count (nth grid x)))
        :let [cell (nth (nth grid x) y)]
        :when (contains? unblocked-chs cell)]
    [x y]))

(defn cheating-possibilities
  [grid costs pos]
  (let [pos-cost (costs pos)
        in-range (in-range-cells grid pos)]
    (filter
     (partial <= cost-target)
     (map (fn [[n-pos distance]] (- (costs n-pos) pos-cost distance)) in-range))))

(defn run
  [inputs]
  (let [grid (mapv vec inputs)
        start (find-coordinates grid \S)
        all-unblocked (unblocked-pos grid)
        costs (shortest-path grid start)]
    (count (apply concat (map (partial cheating-possibilities grid costs) all-unblocked)))))
