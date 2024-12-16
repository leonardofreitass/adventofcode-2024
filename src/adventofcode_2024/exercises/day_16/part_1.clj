(ns adventofcode-2024.exercises.day-16.part-1
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def directions
  {0 [-1 0]  ; North
   1 [0 1]   ; East
   2 [1 0]   ; South
   3 [0 -1]  ; West
   })

(def cost-turn 1000)
(def cost-forward 1)

(defn turn-cost [from-dir to-dir]
  (let [diff (mod (- to-dir from-dir) 4)]
    (case diff
      0 0
      1 cost-turn
      3 cost-turn
      (* 2 cost-turn))))

(defn side-positions [pos dir cost]
  (map (fn [[new-dir move]]
         {:pos (mapv + pos move)
          :dir new-dir
          :cost (+ cost cost-forward (turn-cost dir new-dir))})
       directions))

(defn find-coordinates [grid value]
  (first
   (for [x (range (count grid))
         y (range (count (nth grid x)))
         :let [cell (nth (nth grid x) y)]
         :when (= cell value)]
     [x y])))

(defn run
  [inputs]
  (let [grid (mapv vec inputs)
        start (find-coordinates grid \S)
        end (find-coordinates grid \E)]
    (loop [queue (priority-map {:pos start :dir 1 :cost 0} 0)
           costs {}]
      (if (empty? queue)
        (costs [end 1])
        (let [current (peek queue)
              {:keys [pos dir cost]} (key current)
              queue (pop queue)]
          (if (= pos end)
            cost
            (let [next-positions (filter
                                  (fn [{:keys [pos dir cost]}]
                                    (and (not= \# (get-in grid pos))
                                         (or (nil? (get costs [pos dir]))
                                             (< cost (get costs [pos dir])))))
                                  (side-positions pos dir cost))]
              (recur
               (reduce (fn [q n] (assoc q n (:cost n))) queue next-positions)
               (reduce (fn [c {:keys [pos dir cost]}]
                         (assoc c [pos dir] cost))
                       costs next-positions)))))))))

