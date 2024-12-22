(ns adventofcode-2024.exercises.day-22.part-2)


(defn big-xor
  [a b]
  (.xor (biginteger a) (biginteger b)))

(defn prune [n] (mod n 16777216))
(defn op-mix-and-prune [op n secret]
  (prune (big-xor (bigint (op secret n)) secret)))

(defn next-secret
  [secret]
  (->> secret
       (op-mix-and-prune * 64)
       (op-mix-and-prune / 32)
       (op-mix-and-prune * 2048)))

(defn last-digit
  [n]
  (Integer/parseInt (subs (str n) (dec (count (str n))))))

(defn buyers-secrets
  [inputs]
  (map #(second (reduce
                 (fn [[secret acc] _]
                   (let [n (next-secret secret)]
                     [n (conj acc (last-digit n))]))
                 [(Integer/parseInt %)
                  [(last-digit %)]]
                 (range 0 2000)))
       inputs))

(defn seq-diff
  [s]
  (loop [c (first s)
         s (next s)
         d '()]
    (if (empty? s)
      d
      (recur (first s)
             (next s)
             (conj d (- c (first s)))))))

(defn buyers-diff
  [secrets]
  (reduce
   (fn [m s]
     (assoc m (seq-diff s) (first s)))
   {}
   (partition 5 1 (reverse secrets))))

(defn run
  [inputs]
  (let [secrets (buyers-secrets inputs)
        buy-options (map buyers-diff secrets)
        all-diffs (set (mapcat keys buy-options))]
    (reduce
     (fn [acc d]
       (let [buy (reduce + (map #(get % d 0) buy-options))]
         (max acc buy)))
     0
     all-diffs)))
