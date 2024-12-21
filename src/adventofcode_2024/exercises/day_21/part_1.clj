(ns adventofcode-2024.exercises.day-21.part-1 
  (:require [clojure.data.priority-map :as pm]))

(def numeric-pad-graph
  {\7 {\4 [\v 10] \8 [\> 100]}
   \8 {\7 [\< 4] \5 [\v 11] \9 [\> 100]}
   \9 {\8 [\< 4] \6 [\v 12]}
   \4 {\1 [\v 10] \7 [\^ 10] \5 [\> 100]}
   \5 {\4 [\< 3] \2 [\v 11] \8 [\^ 11] \6 [\> 100]}
   \6 {\5 [\< 3] \3 [\v 12] \9 [\^ 12]}
   \1 {\4 [\^ 10] \2 [\> 100]}
   \2 {\1 [\< 2] \5 [\^ 11] \0 [\v 11] \3 [\> 100]}
   \3 {\2 [\< 2] \6 [\^ 12] \A [\v 12]}
   \0 {\2 [\^ 11] \A [\> 100]}
   \A {\0 [\< 1] \3 [\^ 12]}})

(def directions [\< \^ \v \> \A]) 

(def directional-pad-graph
  {\< {\v [\> 100]}
   \^ {\A [\> 100] \v [\v 10]}
   \v {\< [\< 2] \^ [\^ 10] \> [\> 100]}
   \A {\^ [\< 1] \> [\v 11]}
   \> {\v [\< 2] \A [\^ 11]}})

(def path-with-fewest-turns
  (memoize (fn [graph start end]
             (let [pq (pm/priority-map [start nil ""] 0)
                   dist (atom {[start nil] 0})]
               (loop [pq pq]
                 (if (empty? pq)
                   nil
                   (let [[[node last-dir directions] cost] (peek pq)
                         next-pq (pop pq)]
                     (cond
                       (= node end)
                       (vec directions)
                       :else
                       (let [neighbors (graph node)]
                         (recur
                          (reduce
                           (fn [pq [nbr [dir turn-cost]]]
                             (let [new-cost (if (= dir last-dir)
                                              cost
                                              (+ cost 10000 turn-cost))
                                   old-cost (get @dist [nbr dir] ##Inf)]
                               (if (< new-cost old-cost)
                                 (do
                                   (swap! dist assoc [nbr dir] new-cost)
                                   (assoc pq [nbr dir (str directions dir)] new-cost))
                                 pq)))
                           next-pq
                           neighbors)))))))))))

(defn numeric-to-directional
  [number]
  (first (reduce
          (fn [[acc current] digit]
            [(conj (into acc (path-with-fewest-turns numeric-pad-graph current digit)) \A)
             digit])
          [[] \A]
          (vec number))))

(def pre-computed-directional-costs
  (reduce
   (fn [acc [from to]]
     (assoc acc [from to] (conj (path-with-fewest-turns directional-pad-graph from to) \A)))
   {}
   (for [from directions
         to directions]
     [from to])))

(defn directional-to-directional
  [commands]
  (first (reduce
          (fn [[acc current] command]
            [(into acc (pre-computed-directional-costs [current command]))
             command])
          [[] \A]
          commands)))

(defn run
  [inputs]
  (reduce
   +
   (map #(->> %
              (numeric-to-directional)
              (directional-to-directional)
              (directional-to-directional)
              (apply str)
              (count)
              (* (Integer/parseInt (subs % 0 3)))
              )
        inputs))
  )
