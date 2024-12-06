(ns adventofcode-2024.exercises.day-6.part-1
  (:require [clojure.string :as str]))

(def directions {
  "^" {:move [-1 0] :rotation ">"}
  "v" {:move [1 0] :rotation "<"}
  "<" {:move [0 -1] :rotation "^"}
  ">" {:move [0 1] :rotation "v"}
})

(defn find-start-position [inputs]
  (some (fn [[row-index line]]
          (let [col-index (some (fn [[col-index char]]
                                  (when (#{\^ \> \< \v} char) col-index))
                                (map-indexed vector line))]
            (when col-index [row-index col-index])))
        (map-indexed vector inputs)))

(defn traverse-grid
  [grid start]
  (loop [traversed #{}
         pos start
         ch (get-in grid start)]
    (if (nil? (get-in grid pos))
      traversed
      (let [dir (directions ch)
            nxt (map + pos (:move dir))
            nxt-ch (get-in grid nxt)
            blocked? (= nxt-ch "#")
            ]
        (recur
         (conj traversed pos)
         (if blocked? pos nxt)
         (if blocked? (:rotation dir) ch))))))

(defn run
  [inputs]
  (let [grid (mapv #(str/split % #"") inputs)
        start (find-start-position inputs)]
    (count (traverse-grid grid start))))
