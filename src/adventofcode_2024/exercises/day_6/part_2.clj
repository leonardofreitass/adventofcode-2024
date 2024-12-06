(ns adventofcode-2024.exercises.day-6.part-2
  (:require [clojure.string :as str]))

(def directions {"^" {:move [-1 0] :rotation ">"}
                 "v" {:move [1 0] :rotation "<"}
                 "<" {:move [0 -1] :rotation "^"}
                 ">" {:move [0 1] :rotation "v"}})

(defn find-start-position [inputs]
  (some (fn [[row-index line]]
          (let [col-index (some (fn [[col-index char]]
                                  (when (#{\^ \> \< \v} char) col-index))
                                (map-indexed vector line))]
            (when col-index [row-index col-index])))
        (map-indexed vector inputs)))

(defn all-pos
  [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= (get-in grid [x y]) ".")]
    [x y]))

(defn looping?
  [grid start]
  (loop [traversed #{}
         pos start
         ch (get-in grid start)]
    (if (contains? traversed (conj pos ch))
      true
      (if (nil? (get-in grid pos))
        false
        (let [dir (directions ch)
              nxt (map + pos (:move dir))
              nxt-ch (get-in grid nxt)
              blocked? (= nxt-ch "#")]
          (recur
           (conj traversed (conj pos ch))
           (if blocked? pos nxt)
           (if blocked? (:rotation dir) ch)))))))

(defn run
  [inputs]
  (let [grid (mapv #(str/split % #"") inputs)
        start (find-start-position inputs)]
    (count (filter
            (fn [pos]
              (looping? (assoc-in grid pos "#") start))
            (all-pos grid)))))
