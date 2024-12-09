(ns adventofcode-2024.exercises.day-9.part-2
  (:require [clojure.string :as str]))

(defn non-nil
  [n]
  (if (nil? n) 0 n))

(defn parse-inputs
  [inputs]
  (reduce
   (fn [[idx offset used free] [a b]]
     [(inc idx)
      (+ offset (non-nil a) (non-nil b))
      (if (nil? a) used (conj used {:offset offset :idx idx :size a}))
      (if (nil? b) free (conj free {:offset (+ offset a) :size b}))])
   [0 0 [] []]
   (partition 2 2 nil (mapv #(Integer/parseInt %) (str/split (first inputs) #"")))))

(defn shift-disk
  [used free]
  (loop [remaining (reverse used)
         parsed []
         available free]
    (if (empty? remaining)
      parsed
      (let [block (first remaining)
            next-free (some #(when (and (<= (:size block) (:size %))
                                        (> (:offset block) (:offset %)))
                               %)
                            available)
            updated-available (filterv #(not= % next-free) available)
            next-available (if next-free
                             (if (= (:size block) (:size next-free))
                               updated-available
                               (sort-by :offset (conj updated-available
                                                      {:offset (+ (:offset next-free) (:size block))
                                                       :size (- (:size next-free) (:size block))})))
                             available)]
        (recur (rest remaining)
               (conj parsed
                     (if next-free
                       (assoc block :offset (:offset next-free))
                       block))
               next-available)))))

(defn run
  [inputs]
  (let [[_ _ used free] (parse-inputs inputs)
        disk (shift-disk used free)]
    (reduce
     (fn [acc block]
       (+
        acc
        (reduce
         +
         (map
          *
          (repeat (:idx block))
          (range (:offset block) (+ (:offset block) (:size block)))))))
     0
     disk)))
