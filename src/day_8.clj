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
    [opcode operand]))

(def input (->> "resources/day-8.input"
                slurp
                clojure.string/split-lines))

(defn cycle [machine]
  (let [[opcode operand] (nth (:instructions machine) (:pc machine))
        instruction (operators opcode)]
    (instruction operand machine)))

(defn part-1 [input]
  (loop [visited #{} machine (OpMachine. (mapv parse-instruction input) 0 0)]
    (if (visited (:pc machine))
      (:acc machine)
      (recur (conj visited (:pc machine)) (cycle machine)))))

(def test-input ["nop +0" "acc +1" "jmp +4" "acc +3" "jmp -3"
                 "acc -99" "acc +1" "jmp -4" "acc +6"])

(deftest part-1-test
  (is (= 5 (part-1 test-input))))

(defn part-2-run [input]
  (loop [visited #{} machine (OpMachine. input 0 0)]
    (cond (visited (:pc machine)) false
          (= (count (:instructions machine)) (:pc machine)) (:acc machine)
          :else (recur (conj visited (:pc machine)) (cycle machine)))))

(def part-2-mutations
  {"jmp" "nop" "nop" "jmp" "acc" "acc"})

(defn mutate [mutations input]
  (let [mutator (fn [[opcode operand]]
                  (vector (mutations opcode) operand))]
    (remove #(= input %)
            (map-indexed (fn [index _] (update input index mutator)) input))))
  
(defn part-2 [input]
  (let [mutations (mutate part-2-mutations (mapv parse-instruction input))]
    (some part-2-run mutations)))

(deftest part-2-test
  (is (= 8 (part-2 test-input))))
