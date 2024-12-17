(ns adventofcode-2024.exercises.day-17.part-2)

(defn big-xor
  [a b]
  (.xor (biginteger a) (biginteger b))) ; If you wonder why I am casting from bigint to biginteger, it is because the .xor method is not defined for bigint (the clojure native/faster bigint), only for biginteger (the java class). This later gets converted back to bigint when an operation with another bigint is performed.

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
          0 (recur (bigint (/ reg-a (Math/pow 2 combo-operand))) reg-b reg-c default-next-index output)
          1 (recur reg-a (big-xor reg-b operand) reg-c default-next-index output)
          2 (recur reg-a (bigint (mod combo-operand 8)) reg-c default-next-index output)
          3 (if (zero? reg-a)
              (recur reg-a reg-b reg-c default-next-index output)
              (recur reg-a reg-b reg-c operand output))
          4 (recur reg-a (big-xor reg-b reg-c) reg-c default-next-index output)
          5 (recur reg-a reg-b reg-c default-next-index (conj output (bigint (mod combo-operand 8))))
          6 (recur reg-a (bigint (/ reg-a (Math/pow 2 combo-operand))) reg-c default-next-index output)
          7 (recur reg-a reg-b (bigint (/ reg-a (Math/pow 2 combo-operand))) default-next-index output))))))

(def power 8)

(defn tail-equality
  [program output]
  (loop [program (reverse program)
         output (reverse output)
         result '()]
    (if (or (empty? program) (empty? output))
      result
      (if (= (peek program) (peek output))
        (recur (rest program) (rest output) (conj result (peek program)))
        result))))

(defn run
  [inputs]
  (let [[_ b c _ program] inputs
        register-b (bigint (re-find #"\d+" b))
        register-c (bigint (re-find #"\d+" c))
        program (mapv bigint (re-seq #"-?\d+" program))]
    (loop [register-a (bigint power)]
      (let [output (run-until-halt program register-a register-b register-c)
            tail (tail-equality program output)]
        (if (= program output)
          (str register-a)
          (if (= (count program) (count output))
            (recur (+ register-a (bigint (Math/pow 8 (dec (- (count program) (count tail)))))))
            (recur (* register-a power))))))))
