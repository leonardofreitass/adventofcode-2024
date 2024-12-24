(ns adventofcode-2024.exercises.day-24.part-1 
  (:require
    [clojure.string :as str]))

(defn one? [x] (= x 1))

(defn btoi [b] (if b 1 0))

(def op-map
  {
   "OR" #(or (one? %1) (one? %2))
   "AND" #(and (one? %1) (one? %2))
   "XOR"#(not= %1 %2)
   })

(defn do-op
  [op & args]
  (btoi (apply (op-map op) args)))

(defn parse-wires
  [wires]
  (reduce
   (fn [m [k v]]
     (assoc m k (Integer/parseInt v)))
   {}
   (map #(str/split % #": ") wires)))

(defn parse-gates
  [gates]
  (reduce
   (fn [m [a op b _ g]]
     (assoc m g [op a b]))
   {}
   (map #(str/split % #" ") gates)))

(defn calc-wire
  [wires gates wire]
  (or (wires wire)
      (let [[op a b] (gates wire)]
        (do-op op (calc-wire wires gates a) (calc-wire wires gates b)))))

(defn run
  [inputs]
  (let [[_initial-wires _gates] (split-with (partial not= "") inputs)
        wires (parse-wires _initial-wires)
        gates (parse-gates (next _gates))
        z-gates (reverse (sort (filter #(str/starts-with? % "z") (keys gates))))]
    (BigInteger.
     (str/join (map #(calc-wire wires gates %) z-gates))
     2)))
