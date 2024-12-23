(ns adventofcode-2024.exercises.day-23.part-2 
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

(defn plot-graph
  [inputs]
  (reduce
   (fn [m [a b]]
     (-> m
         (update a (fnil conj #{}) b)
         (update b (fnil conj #{}) a)))
   {}
   (map #(str/split % #"-") inputs)))

(defn find-groups
  [graph]
  (reduce-kv
   (fn [acc a s]
     (assoc acc a (set (map #(conj (set/intersection s (graph %)) % a) s))))
   {}
   graph))

(defn filter-connected-groups
  [groups]
  (filter
   (fn [group]
     (every? #(contains? (groups %) group) group))
   (set (apply concat (vals groups)))))

(defn run
  [inputs]
  (let [graph (plot-graph inputs)
        groups (find-groups graph)]
    (str/join "," (sort (last (sort-by count (filter-connected-groups groups)))))))
