(ns adventofcode-2024.exercises.day-17.part-1 
  (:require
    [clojure.string :as str]))

(defn combo-operands
  [register-a register-b register-c operand]
  (case operand
    4 register-a
    5 register-b
    6 register-c
    7 nil
    operand))

(defn run-until-halt
  [program register-a register-b register-c index output]
  (let [opcode (get program index)
        operand (get program (inc index))
        combo-operand (combo-operands register-a register-b register-c operand)
        default-next-index (+ 2 index)
        ruh (partial run-until-halt program)]
    (if (nil? opcode)
      output
      (case opcode
        0 (ruh (int (/ register-a (Math/pow 2 combo-operand))) register-b register-c default-next-index output)
        1 (ruh register-a (bit-xor register-b operand) register-c default-next-index output)
        2 (ruh register-a (int (mod combo-operand 8)) register-c default-next-index output)
        3 (if (zero? register-a)
            (ruh register-a register-b register-c default-next-index output)
            (ruh register-a register-b register-c operand output))
        4 (ruh register-a (bit-xor register-b register-c) register-c default-next-index output)
        5 (ruh register-a register-b register-c default-next-index (conj output (int (mod combo-operand 8))))
        6 (ruh register-a (int (/ register-a (Math/pow 2 combo-operand))) register-c default-next-index output)
        7 (ruh register-a register-b (int (/ register-a (Math/pow 2 combo-operand))) default-next-index output)))))

(defn run
  [inputs]
  (let [[a b c _ program] inputs
        register-a (Integer/parseInt (re-find #"\d+" a))
        register-b (Integer/parseInt (re-find #"\d+" b))
        register-c (Integer/parseInt (re-find #"\d+" c))
        program (mapv #(Integer/parseInt %) (re-seq #"-?\d+" program))]
    (str/join "," (run-until-halt program register-a register-b register-c 0 []))))
