(ns adventofcode-2024.exercises.day-23.part-1 
  (:require
    [clojure.string :as str]))

(defn plot-graph
  [inputs]
  (reduce
   (fn [m [a b]]
     (-> m
         (update a (fnil conj #{}) b)
         (update b (fnil conj #{}) a)))
   {}
   (map #(str/split % #"-") inputs)))

(defn pair-combinations [s]
  (for [x s
        y s
        :when (and (not= x y)
                   (< (compare x y) 0))]
    [x y]))

(defn find-trios
  [graph]
  (reduce-kv
   (fn [acc a s]
     (let [combinations (pair-combinations s)]
       (apply
        conj acc
        (map #(conj (set %) a)
             (filter (fn [[l r]] (contains? (graph l) r)) combinations)))))
   #{}
   graph))

(defn run
  [inputs]
  (let [graph (plot-graph inputs)
        trios (find-trios graph)]
    (count (filter
            (fn [trio]
              (some #(str/starts-with? % "t") trio))
            trios))))
