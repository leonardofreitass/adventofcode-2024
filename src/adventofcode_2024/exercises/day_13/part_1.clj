(ns adventofcode-2024.exercises.day-13.part-1)

(defn get-numbers
  [str]
  (mapv #(Integer/parseInt %) (re-seq #"\d+" str)))

(defn parse-block
  [[a b prize]]
  {:a (get-numbers a)
   :b (get-numbers b)
   :prize (get-numbers prize)})

(defn parse-inputs
  [inputs]
  (map
   parse-block
   (partition 3 4 inputs)))

(defn solve-block
  [{:keys [a b prize]}]
  (let [[ax ay] a
        [bx by] b
        [px py] prize
        an (abs (/ (- (* bx py) (* by px)) (- (* by ax) (* bx ay))))
        bn (/ (- py (* ay an)) by)]
    (if (or (> an 100) (> bn 100))
      0
      (+ (* 3 an) bn))))

(defn run
  [inputs]
  (reduce
   +
   (filter
    integer?
    (map
     solve-block
     (parse-inputs inputs)))))
