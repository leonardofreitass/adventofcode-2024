(ns adventofcode-2024.exercises.day-19.part-1 
  (:require [clojure.string :as str]))

(defn parse-inputs
  [inputs]
  (let [[patterns-str _ & designs] inputs
        patterns (re-seq #"\w+" patterns-str)]
    [(set patterns) designs]))

(defn possible-design?
  [patterns design]
  (if (empty? design)
    true
    (some
     #(if (str/starts-with? design %)
        (possible-design? patterns (subs design (count %))))
     patterns)))

(defn run
  [inputs]
  (let [[patterns designs] (parse-inputs inputs)]
    (count (filter #(possible-design? patterns %) designs))))
