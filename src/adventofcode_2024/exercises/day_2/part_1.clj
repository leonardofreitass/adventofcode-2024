(ns adventofcode-2024.exercises.day-2.part-1
  (:require [clojure.string :as str]))

(defn parse-inputs
  [inputs]
  (map (fn [line] (map #(Integer/parseInt %) (str/split line #" "))) inputs))

(defn safe?
  [line]
  (loop [ld 0
         l (first line)
         n (second line)
         s (next (next line))]
    (let [diff (- l n)
          faulty (or
                  (zero? diff)
                  (> (Math/abs diff) 3)
                  (and (not (zero? ld)) (neg? (bit-xor diff ld))))]
      (cond
        faulty false
        (empty? s) true
        :else (recur diff n (first s) (next s))))))

(defn run
  [inputs]
  (reduce
   (fn [acc line]
     (if (safe? line)
       (inc acc)
       acc))
   0
   (parse-inputs inputs)))
