(ns adventofcode-2024.exercises.day-20.part-1
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def directions [[-1 0] [0 1] [1 0] [0 -1]])

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

(defn find-cheat-possibilities [grid]
  (for [x (range (count grid))
             y (range (count (nth grid x)))
             :let [cell (nth (nth grid x) y)
                   sides (cond
                           (and (contains? unblocked-chs (get-in grid [x (dec y)]))
                                (contains? unblocked-chs (get-in grid [x (inc y)]))) [[x (dec y)] [x (inc y)]]
                           (and (contains? unblocked-chs (get-in grid [(dec x) y]))
                                (contains? unblocked-chs (get-in grid [(inc x) y]))) [[(dec x) y] [(inc x) y]]
                           :else nil)]
             :when (and (= cell \#)
                        (not (nil? sides)))]
         {:pos [x y] :sides sides}))

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

(defn run
  [inputs]
  (let [grid (mapv vec inputs)
        start (find-coordinates grid \S)
        all-blocks (find-cheat-possibilities grid)
        costs (shortest-path grid start)]
    (count (filter (partial <= 100) (map
                    (fn [{:keys [sides]}]
                      (- (abs (reduce - (map costs sides))) 2))
                    all-blocks)))))

