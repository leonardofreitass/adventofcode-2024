(ns adventofcode-2024.exercises.day-17.part-1
  (:require [clojure.string :as str]))

(defn combo-operands
  [register-a register-b register-c operand]
  (case operand
    4 register-a
    5 register-b
    6 register-c
    7 nil
    operand))

(defn run-until-halt
  [program register-a register-b register-c]
  (loop [reg-a register-a
         reg-b register-b
         reg-c register-c
         index 0
         output []]
    (let [opcode (get program index)
          operand (get program (inc index))
          combo-operand (combo-operands reg-a reg-b reg-c operand)
          default-next-index (+ 2 index)]
      (if (nil? opcode)
        output
        (case opcode
          0 (recur (int (/ reg-a (Math/pow 2 combo-operand))) reg-b reg-c default-next-index output)
          1 (recur reg-a (bit-xor reg-b operand) reg-c default-next-index output)
          2 (recur reg-a (int (mod combo-operand 8)) reg-c default-next-index output)
          3 (if (zero? reg-a)
              (recur reg-a reg-b reg-c default-next-index output)
              (recur reg-a reg-b reg-c operand output))
          4 (recur reg-a (bit-xor reg-b reg-c) reg-c default-next-index output)
          5 (recur reg-a reg-b reg-c default-next-index (conj output (int (mod combo-operand 8))))
          6 (recur reg-a (int (/ reg-a (Math/pow 2 combo-operand))) reg-c default-next-index output)
          7 (recur reg-a reg-b (int (/ reg-a (Math/pow 2 combo-operand))) default-next-index output))))))

(defn run
  [inputs]
  (let [[a b c _ program] inputs
        register-a (Integer/parseInt (re-find #"\d+" a))
        register-b (Integer/parseInt (re-find #"\d+" b))
        register-c (Integer/parseInt (re-find #"\d+" c))
        program (mapv #(Integer/parseInt %) (re-seq #"-?\d+" program))]
    (str/join "," (run-until-halt program register-a register-b register-c))))
