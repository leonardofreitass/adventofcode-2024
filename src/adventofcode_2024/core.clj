(ns adventofcode-2024.core
  (:require [adventofcode-2024.exercises :as exercises])
  (:gen-class))

(defn -main
  "Runs an advent of code exercise"
  [& [exercise part]]
  (exercises/execute exercise part))
