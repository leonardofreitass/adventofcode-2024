(ns adventofcode-2024.exercises.day-15.part-2
  (:require [clojure.string :as str]))

(def robot-ch \@)
(def left-box-ch \[)
(def right-box-ch \])

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

(defn widen-grid
  [grid]
  (mapv
   (fn [row]
     (vec (apply
           concat
           (mapv
            (fn [cell]
              (case cell
                \# [\# \#]
                \. [\. \.]
                \O [\[ \]]
                \@ [\@ \.]))
            row))))
   grid))

(defn parse-inputs
  [inputs]
  (let [[_grid _moves] (split-with (partial not= "") inputs)
        grid (widen-grid (mapv vec _grid))
        moves (str/join "" (next _moves))]
    [(map-grid grid) moves]))

(defn move-objects
  [grid to-arr from-arr]
  (reduce
   (fn [grid [to from]]
     (dissoc (assoc grid to (grid from)) from))
   grid
   (map vector to-arr from-arr)))

(defn expand-from
  [grid from]
  (let [left (grid (first from))
        right (grid (last from))]
    (vec (concat
          (if (= left right-box-ch) [(map + (first from) (directions \<))] [])
          from
          (if (= right left-box-ch) [(map + (last from) (directions \>))] [])))))

(defn shift
  [grid dir _from]
  (let [moving-vertically (contains? #{(directions \^) (directions \v)} dir)
        expanded-from (if moving-vertically (expand-from grid _from) _from)
        from (if moving-vertically (filter #(not (nil? (grid %))) expanded-from) expanded-from)
        to (mapv #(mapv + % dir) from)]
    (if (some #(not (nil? (grid %))) to)
      (if (every? #(contains? #{left-box-ch right-box-ch nil} (grid %)) to)
        (let [[new-grid pushed] (shift grid dir to)]
          [(if pushed (move-objects new-grid to from) grid) pushed])
        [grid false])
      [(move-objects grid to from) true])))

(defn iterate-moves
  [grid robot moves]
  (:grid (reduce
          (fn [{:keys [grid robot]} move]
            (let [dir (directions move)
                  [new-grid pushed] (shift grid dir [robot])]
              {:grid new-grid :robot (if pushed (mapv + robot dir) robot)}))
          {:grid grid :robot robot}
          moves)))

(defn run
  [inputs]
  (let [[{:keys [grid robot]} moves] (parse-inputs inputs)
        final-grid (iterate-moves grid robot moves)]
    (reduce-kv
     (fn [acc [y x] cell]
       (if (= cell left-box-ch)
         (+ acc (+ (* 100 y) x))
         acc))
     0
     final-grid)))
