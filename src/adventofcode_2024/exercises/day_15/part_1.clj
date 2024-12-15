(ns adventofcode-2024.exercises.day-15.part-1 
  (:require [clojure.string :as str]))

(def robot-ch \@)
(def box-ch \O)

(def directions
  {\^ [-1 0]
   \v [1 0]
   \< [0 -1]
   \> [0 1]})

(defn map-grid
  [grid]
  (reduce
   (fn [{:keys [grid robot]} [x y cell]]
     {:grid (assoc grid [y x] cell)
      :robot (if (= cell robot-ch) [y x] robot)})
   {:grid {} :robot nil}
   (for [y (range (count grid))
         x (range (count (first grid)))
         :let [cell (get-in grid [y x])]
         :when (not= cell \.)]
     [x y cell])))

(defn parse-inputs
  [inputs]
  (let [[_grid _moves] (split-with (partial not= "") inputs)
        grid (mapv vec _grid)
        moves (str/join "" (next _moves))]
    [(map-grid grid) moves]))

(defn move-objects
  [grid to from]
  (dissoc (assoc grid to (grid from)) from))


(defn shift
  [grid dir from]
  (let [to (mapv + from dir)
        to-ch (grid to)]
    (if (not (nil? to-ch))
      (if (= to-ch box-ch)
        (let [[new-grid pushed] (shift grid dir to)]
          (if pushed [(move-objects new-grid to from) true] [grid false]))
        [grid false])
      [(move-objects grid to from) true])))

(defn iterate-moves
  [grid robot moves]
  (:grid (reduce
          (fn [{:keys [grid robot]} move]
            (let [dir (directions move)
                  [new-grid pushed] (shift grid dir robot)]
              {:grid new-grid :robot (if pushed (mapv + robot dir) robot)}))
          {:grid grid :robot robot}
          moves)))

(defn run
  [inputs]
  (let [[{:keys [grid robot]} moves] (parse-inputs inputs)
        final-grid (iterate-moves grid robot moves)]
    (reduce-kv
     (fn [acc [y x] cell]
       (if (= cell box-ch)
         (+ acc (+ (* 100 y) x))
         acc))
     0
     final-grid)))
