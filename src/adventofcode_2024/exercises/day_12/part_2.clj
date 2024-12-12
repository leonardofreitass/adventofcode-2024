(ns adventofcode-2024.exercises.day-12.part-2)

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

(defn generate-diagonal-neighbours
  [[x y]]
  [[(dec x) (dec y)] [(inc x) (dec y)] [(dec x) (inc y)] [(inc x) (inc y)]])

(defn reflection [[cx cy] [x1 y1] [x2 y2]]
  (let [dx1 (- x1 cx)
        dy1 (- y1 cy)
        dx2 (- x2 cx)
        dy2 (- y2 cy)]
    [(- cx (+ dx1 dx2)) (- cy (+ dy1 dy2))]))

(defn opposite-side-neighbours [[cx cy] [nx ny]]
  (let [dx (- nx cx)
        dy (- ny cy)]
    [[(if (zero? dx) (inc cx) (+ cx (* -1 dx))) (if (zero? dy) (inc cy) (+ cy (* -1 dy)))]
     [(if (zero? dx) (dec cx) (+ cx (* -1 dx))) (if (zero? dy) (dec cy) (+ cy (* -1 dy)))]]))


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

(defn side-contribution
  [grid pos]
  (let [ch (get-in grid pos)
        neighbours (filter #(not= ch (get-in grid %)) (generate-neighbours pos))
        diagonal-neighbours (filter #(not= ch (get-in grid %)) (generate-diagonal-neighbours pos))]
    (case (count neighbours)
      0 (count diagonal-neighbours)
      1 (count (filter #(not= ch (get-in grid %)) (opposite-side-neighbours pos (first neighbours))))
      2 (if
         (= [1 1] (apply mapv #(abs (- %1 %2)) neighbours))
          (if (not= ch (get-in grid (apply (partial reflection pos) neighbours))) 2 1) 0)
      3 2
      4 4)))

(defn group-value
  [grid group]
  (*
   (count group) 
   (reduce +
           (map
            (partial side-contribution grid)
            group))))

(defn run
  [inputs]
  (let [grid (parse-inputs inputs)]
    (reduce
     +
     (map (partial group-value grid) (count-groups grid)))))
