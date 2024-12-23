(ns adventofcode-2024.exercises)

(defn exercise-to-ns
  "Gets namespace from exercise"
  [exercise part]
  (symbol (str "adventofcode-2024.exercises.day-" exercise ".part-" part)))

(defn exercise-to-input-file
  "Gets input file path from exercise"
  [exercise]
  (str "./resources/day_" exercise "_inputs"))

(defn execute 
  "Handler for executing exercises"
  [exercise part]
  (println "Executing exercise from day" exercise "part" part)
  (require (exercise-to-ns exercise part))
  (let [exercise-ns (find-ns (exercise-to-ns exercise part))]
    (with-open [input-file (clojure.java.io/reader (exercise-to-input-file exercise))]
      (let [solution (time ((ns-resolve exercise-ns 'run) (line-seq input-file)))]
        (println "Solution" solution)))))
