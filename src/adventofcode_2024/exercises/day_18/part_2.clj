(ns adventofcode-2024.exercises.day-18.part-2
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str]))

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
  (map (fn [line] (mapv #(Integer/parseInt %) (re-seq #"\d+" line))) inputs))

(defn shortest-path
  [blocks start end]
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
            (recur (assoc-pos queue next-positions) (assoc-pos costs next-positions))))))))

(defn run
  [inputs]
  (let [all-blocks (parse-inputs inputs)
        start [0 0]
        end [bounds bounds]]
    (loop [blocks (take n-bytes all-blocks)
           to-add (drop n-bytes all-blocks)]
      (let [shortest (shortest-path (set blocks) start end)]
        (if (nil? shortest)
          (str/join "," (first blocks))
          (recur (conj blocks (first to-add)) (rest to-add)))))))
