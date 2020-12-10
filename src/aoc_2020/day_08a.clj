(ns aoc-2020.day-08a
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "input-day-08a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "nop +0"
                     "acc +1"
                     "jmp +4"
                     "acc +3"
                     "jmp -3"
                     "acc -99"
                     "acc +1"
                     "jmp -4"
                     "acc +6"))

(defn parse-instruction [line]
  (let [[_ op param] (re-matches #"(\w\w\w) ((?:\+|\-)\d+)" line)]
    [(keyword op) (read-string param)]))

(defn instr-pointer [state] (state :instruction-pointer))
(defn inc-instr-pointer [state x]
  (let [current (state :instruction-pointer)]
    (assoc state :instruction-pointer (+ current x))))
(defn inc-accumulator [state x]
  (let [current (state :accumulator)]
    (assoc state :accumulator (+ current x))))

(defn create-boot-loader [code-lines]
  {:state      {:accumulator         0
                :instruction-pointer 0}
   :history    #{}
   :operations {:nop (fn [state _] (inc-instr-pointer state 1))
                :jmp inc-instr-pointer
                :acc (fn [state p] (-> state (inc-accumulator p) (inc-instr-pointer 1)))}
   :program    (->> (map parse-instruction code-lines) vec)})

(defn read-new-instruction [boot-loader]
  (let [program (boot-loader :program)
        state (boot-loader :state)
        history (boot-loader :history)
        instr (instr-pointer state)]
    (if-not (history instr)
      (-> program
        (get (instr-pointer state))))))

(defn get-operation [boot-loader operation-type]
  ((boot-loader :operations) operation-type))

(defn execute [op state param] (op state param))

(defn record-executed [new-state {history :history state :state :as boot-loader}]
  (assoc boot-loader
    :history (conj history (instr-pointer state))
    :state new-state))

(defn run [{state :state :as boot-loader}]
  (if-let [[op-type param] (read-new-instruction boot-loader)]
    (-> (get-operation boot-loader op-type)
      (execute state param)
      (record-executed boot-loader)
      recur)
    state))

(defn solve [input]
  (let [final-state (run (create-boot-loader input))]
    (final-state :accumulator)))

;; TESTS
(defn test-solution [] (= (solve example-input) 5))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))