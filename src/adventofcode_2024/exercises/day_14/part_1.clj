(ns adventofcode-2024.exercises.day-14.part-1)

(def mod-arr [101 103])
(def iterations 100)

(defn pos-quadrant
  [[x y]]
  (let [mid-x (/ (dec (first mod-arr)) 2)
        mid-y (/ (dec (second mod-arr)) 2)]
    (cond
     (and (< x mid-x) (< y mid-y)) 0
     (and (< x mid-x) (> y mid-y)) 1
     (and (> x mid-x) (< y mid-y)) 2
     (and (> x mid-x) (> y mid-y)) 3
      :else nil)))

(defn parse-inputs
  [inputs]
  (map (fn [input] (partition 2 (mapv #(Integer/parseInt %) (re-seq #"-?\d+" input)))) inputs))

(defn pass-seconds
  [robots]
  (map
   (fn [[initial speed]]
     (map
      #(mod (+ %2 (* iterations %3)) %1)
      mod-arr initial speed))
   robots))

(defn run
  [inputs]
  (let [robots (parse-inputs inputs)
        future-robots (pass-seconds robots)
        quadrants (group-by identity (filter some? (map pos-quadrant future-robots)))]
    (reduce
     *
     (map count (vals quadrants)))))
 