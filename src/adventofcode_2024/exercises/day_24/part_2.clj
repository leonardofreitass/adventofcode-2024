(ns adventofcode-2024.exercises.day-24.part-2
  (:require
   [clojure.string :as str]))

(def first-bit "00")
(def last-bit "45")

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
     (assoc m g {:op op :terms #{a b}}))
   {}
   (map #(str/split % #" ") gates)))

(defn validate-gate
  [gates out logic]
  (let [out-w (subs out 0 1)
        out-n (subs out 1)
        {:keys [op terms]} logic]
    (cond
      (= out-w "z") (or (= op "XOR")
                        (and (= op "OR") (= out-n last-bit))) 

      (not (every? #(or (str/starts-with? % "x") (str/starts-with? % "y")) terms)) (contains? #{"AND" "OR"} op)

      (and (= op "XOR") (not (every? #(str/ends-with? % first-bit) terms))) (some (fn [gate] (and (= "XOR" (:op gate)) (contains? (:terms gate) out))) (vals gates))

      (and (= op "AND") (not (every? #(str/ends-with? % first-bit) terms))) (some (fn [gate] (and (= "OR" (:op gate)) (contains? (:terms gate) out))) (vals gates))

      :else true)))

(defn run
  [inputs]
  (let [[_ _gates] (split-with (partial not= "") inputs)
        gates (parse-gates (next _gates))]
    (str/join "," (sort (reduce-kv
                         (fn [m k v]
                           (if (validate-gate gates k v)
                             m
                             (conj m k)))
                         #{}
                         gates)))))
