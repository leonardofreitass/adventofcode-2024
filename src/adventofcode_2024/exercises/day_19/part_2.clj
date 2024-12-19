(ns adventofcode-2024.exercises.day-19.part-2
  (:require [clojure.string :as str]))

(defn parse-inputs
  [inputs]
  (let [[patterns-str _ & designs] inputs
        patterns (re-seq #"\w+" patterns-str)]
    [(set patterns) designs]))

(def count-possible-designs
  (memoize (fn [patterns design]
               (if (empty? design)
                 1
                 (reduce +
                         (map #(if (str/starts-with? design %)
                                 (count-possible-designs patterns (subs design (count %)))
                                 0)
                              patterns))))))

(defn run
  [inputs]
  (let [[patterns designs] (parse-inputs inputs)]
    (reduce + (map (partial count-possible-designs patterns) designs))))
