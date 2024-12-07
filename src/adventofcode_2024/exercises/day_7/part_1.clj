(ns adventofcode-2024.exercises.day-7.part-1)

(defn parse-inputs
  [inputs]
  (map
   #(map bigint (re-seq #"\d+" %))
   inputs))

(defn can-calculate
  [target acc numbers]
  (cond
    (> acc target) false
    (empty? numbers) (= acc target)
    :else (or (can-calculate target (+ acc (first numbers)) (rest numbers))
              (can-calculate target (* acc (first numbers)) (rest numbers)))))

(defn run
  [inputs]
  (reduce
   +
   (bigint 0)
   (map
    first
    (filter
     (fn [[target acc & numbers]]
       (can-calculate target acc numbers))
     (parse-inputs inputs)))))
