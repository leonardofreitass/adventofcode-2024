(ns adventofcode-2024.exercises.day-22.part-1)

(defn big-xor
  [a b]
  (.xor (biginteger a) (biginteger b)))

(defn prune [n] (mod n 16777216))
(defn op-mix-and-prune [op n secret]
  (prune (big-xor (bigint (op secret n)) secret)))

(defn next-secret
  [secret _]
  (->> secret
       (op-mix-and-prune * 64)
       (op-mix-and-prune / 32)
       (op-mix-and-prune * 2048)))

(defn run
  [inputs]
  (reduce
   +
   (map #(reduce
         next-secret
         %
         (range 0 2000))
        (map #(Integer/parseInt %) inputs))))
