(ns adventofcode-2024.exercises.day-9.part-1
  (:require [clojure.string :as str]))

(defn parse-inputs
  [inputs]
  (reduce
   (fn [[used free] [a b]]
     [(if (nil? a) used (conj used a))
      (if (nil? b) free (conj free b))])
   [[] []]
   (partition 2 2 nil (mapv #(Integer/parseInt %) (str/split (first inputs) #"")))))

(defn shift-blocks
  [space block-idx used]
  (if (empty? used)
    [[] used]
    (loop [shifted []
           blocks used
           left (if (nil? space) 0 space)]
      (if (or (zero? left) (= 1 (count blocks)))
        [(flatten (map
                   (fn [[idx n]] (repeat n idx))
                   shifted))
         blocks]
        (let [last-block (last blocks)
              shifting (min left last-block)]
          (recur
           (conj shifted [(dec (+ block-idx (count blocks))) shifting])
           (if (= shifting last-block)
             (pop blocks)
             (update blocks (dec (count blocks)) - shifting))
           (- left shifting)))))))

(defn drop-vec
  [v]
  (if (empty? v)
    []
    (subvec v 1)))

(defn run
  [inputs]
  (let [[used free] (parse-inputs inputs)]
    (loop [block-idx 0
           address-idx 0
           acc 0
           used used
           free free]
      (let [block (first used)
            space (first free)
            block-size (reduce + (map * (repeat block-idx) (range address-idx (+ address-idx block))))
            [shifting new-used] (shift-blocks space block-idx used)
            new-free (subvec free 0 (max 0 (- (count free) (- (count used) (count new-used)))))
            shifted-size (if (empty? shifting) 0 (reduce + (map * shifting (range (+ block address-idx) (+ block address-idx (count shifting))))))
            new-acc (+ acc block-size shifted-size)
            processed-new-used (drop-vec new-used)
            processed-new-free (drop-vec new-free)]
        (if (every? empty? [processed-new-used processed-new-free])
          new-acc
          (recur
           (inc block-idx)
           (+ address-idx block space)
           new-acc
           processed-new-used
           processed-new-free))))))
