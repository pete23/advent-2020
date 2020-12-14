(ns day-8
  (:use clojure.test)
  (:require [clojure.string :refer [split]]))

(defn atol [s] (Long/parseLong s))

(defrecord OpMachine [instructions pc acc])

(defn instruction [pc-f acc-f]
  "Returns a function for a machine instruction. This will take an OpMachine and an operand and return a new OpMachine state."
  (let [pc-f (or pc-f (fn [_ pc] (inc pc)))
        acc-f (or acc-f (fn [_ acc] acc))]
    (fn [operand machine]
      (OpMachine. (. machine instructions)
                  (pc-f operand (. machine pc))
                  (acc-f operand (. machine acc))))))

(def operators {"acc" (instruction nil +)
                "nop" (instruction nil nil)
                "jmp" (instruction + nil) })

(defn parse-instruction [s]
  (let [[opcode operand] (split s #" " 2)
        operand (atol operand)]
    (partial (operators opcode) operand)))


(def input (->> "resources/day-8.input"
                slurp
                clojure.string/split-lines
                (mapv parse-instruction)))

(defn cycle [machine]
  (let [instruction (nth (:instructions machine) (:pc machine))]
    (instruction machine)))

(defn part-1 [input]
  (loop [visited #{} machine (OpMachine. input 0 0)]
    (if (visited (:pc machine))
      (:pc machine)
      (recur (conj visited (:pc machine)) (cycle machine)))))

(def test-input ["nop +0" "acc +1" "jmp +4" "acc +3" "jmp -3"
                 "acc -99" "acc +1" "jmp -4" "acc +6"])

(deftest part-1-test
  (is (= 5 (part-1 (mapv parse-instructions test-input)))))
