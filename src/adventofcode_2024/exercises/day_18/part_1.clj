(ns adventofcode-2024.exercises.day-18.part-1
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def directions [[-1 0] [0 1] [1 0] [0 -1]])
(def bounds 70)
(def n-bytes 1024)

(defn side-positions [pos cost]
  (map (fn [move] [(mapv + pos move) (inc cost)])
       directions))

(defn permitted-pos?
  [pos]
  (every? #(>= bounds % 0) pos))

(defn assoc-pos
  [m l]
  (reduce (fn [a [p c]] (assoc a p c)) m l))

(defn parse-inputs
  [inputs]
  (set (map (fn [line] (mapv #(Integer/parseInt %) (re-seq #"\d+" line))) (take n-bytes inputs))))

(defn run
  [inputs]
  (let [blocks (parse-inputs inputs)
        start [0 0]
        end [bounds bounds]]
    (loop [queue (priority-map start 0)
           costs {}]
      (if (empty? queue)
        (costs end)
        (let [[pos cost] (peek queue)
              queue (pop queue)]
          (if (= pos end)
            cost
            (let [next-positions (filter
                                  (fn [[pos cost]]
                                    (and (not (contains? blocks pos))
                                         (permitted-pos? pos)
                                         (or (nil? (get costs pos))
                                             (< cost (get costs pos)))))
                                  (side-positions pos cost))]
              (recur (assoc-pos queue next-positions) (assoc-pos costs next-positions)))))))))
