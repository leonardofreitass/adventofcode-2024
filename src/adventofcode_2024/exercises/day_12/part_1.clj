(ns adventofcode-2024.exercises.day-12.part-1)

(defn parse-inputs
  [inputs]
  (mapv vec inputs))

(defn all-pos
  [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [x y]))

(defn generate-neighbours
  [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn count-groups
  [grid]
  (let [all-pos (all-pos grid)]
    (loop [pos (first all-pos)
           all (set (rest all-pos))
           queue '()
           group #{(first all-pos)}
           groups []]
      (let [ch (get-in grid pos)
            neighbours (generate-neighbours pos)
            same-ch (filter #(and (= ch (get-in grid %)) (not (contains? group %))) neighbours)
            next-queue (concat (rest queue) same-ch)
            next-all (apply disj (disj all pos) same-ch)
            next-group (if (empty? next-queue)
                         #{(first next-all)}
                         (apply conj group same-ch))
            next-pos (if (empty? next-queue)
                       (first next-all)
                       (first next-queue))
            next-groups (if (empty? next-queue)
                          (conj groups group)
                          groups)]
        (if (empty? next-all)
          next-groups
          (recur next-pos next-all next-queue next-group next-groups))))))

(defn group-value
  [grid group]
  (*
   (count group)
   (reduce +
           (map
            (fn [pos]
              (let [ch (get-in grid pos)
                    neighbours (generate-neighbours pos)]
                (count (filter #(not= ch (get-in grid %)) neighbours))))
            group))))

(defn run
  [inputs]
  (let [grid (parse-inputs inputs)]
    (reduce
     +
     (map (partial group-value grid) (count-groups grid)))))
