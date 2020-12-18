(ns day-18
  (:use clojure.test)
  (:require [clojure.string :refer [split-lines]]))

(def empty-state {:stack [] :op + :acc 0})

(defn push [{:keys [stack op acc] :as state}]
  "Push the current state onto the stack and open a new context"
  (update empty-state :stack conj state))

(defn pull [{:keys [stack op acc] :as state}]
  "Pull the old state from the stack, update acc with op(result). 
   No stack trimming needed as old state already has the popped stack:-)"
  (update (peek stack) :acc (:op (peek stack)) acc))

(defn op [f]
  "Operator sets :op to the operator function"
  (fn [state] (assoc state :op f)))

(defn lit [l]
  "A literal updates acc with (op acc literal)"
  (fn [state] (update state :acc (:op state) l)))

(def char->fn
  {\1 (lit 1) \2 (lit 2) \3 (lit 3) \4 (lit 4) \5 (lit 5)
   \6 (lit 6) \7 (lit 7) \8 (lit 8) \9 (lit 9) \0 (lit 0)
   \( push \) pull
   \* (op *) \+ (op +)})

(def input (->> "resources/day-18.input"
                slurp
                split-lines))

(defn evaluator [s]
  (reduce #(%2 %1) empty-state (filter some? (map char->fn s))))

(defn evaluate [s]
  (:acc (evaluator s)))

(deftest test-evaluate
  (is (= 9 (evaluate "9")))
  (is (= 3 (evaluate "1 + 2")))
  (is (= 6 (evaluate "2 * 3")))
  (is (= 9 (evaluate "1 + 2 * 3")))
  (is (= 71 (evaluate "1 + 2 * 3 + 4 * 5 + 6")))
  (is (= 7 (evaluate "1 + (2 * 3)")))
  (is (= 51 (evaluate "1 + (2 * 3) + (4 * (5 + 6))")))
  (is (= 26 (evaluate "2 * 3 + (4 * 5)")))
  (is (= 437 (evaluate "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
  (is (= 12240 (evaluate "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
  (is (= 13632 (evaluate "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))

(defn part-1 []
  (reduce + (map evaluate input)))

(defn out [l]
  (fn [state]
    (update state :output conj l)))

(def op-prec {* 1 + 0 :open 2 nil 3})

(defn stack->output [state]
  (let [top (peek (:stack state))]
    (-> state
        (update :stack pop)
        (update :output conj top))))

(defn shop [f]
  (let [prec (op-prec f)]
    (fn [{:keys [stack] :as state}]
      (let [top-f (peek stack)
            top-prec (op-prec top-f)]
        (cond (> top-prec prec) (update state :stack conj f)
              (= top-prec prec) (update state :output conj f) ;; only one operator at each precedence level
              (< top-prec prec) (recur (stack->output state)))))))

(defn open [state]
  (update state :stack conj :open))

(defn close [{:keys [stack] :as state}]
    (if (= :open (peek stack))
      (update state :stack pop)
      (recur (stack->output state))))

(def char->shunting-fn
  {\1 (out 1) \2 (out 2) \3 (out 3) \4 (out 4) \5 (out 5)
   \6 (out 6) \7 (out 7) \8 (out 8) \9 (out 9) \0 (out 0)
   \( open \) close
   \* (shop *) \+ (shop +)})

(def empty-shunter {:stack [] :output []})

(defn shunter [s]
  (reduce #(%2 %1)
          empty-shunter
          (filter some? (map char->shunting-fn s))))

(defn shunt-reduce [acc n]
  (cond (number? n) (conj acc n)
        (ifn? n) (let [operands (take-last 2 acc)]
                   (conj (vec (drop-last 2 acc)) (apply n operands)))))

(defn shuvaluate [s]
  (let [{:keys [output stack] :as state} (shunter s)]
    (first (reduce shunt-reduce [] (concat output (reverse stack))))))

(deftest shunter-test
  (is (= 2 (shuvaluate "2")))
  (is (= 4 (shuvaluate "2 + 2")))
  (is (= 6 (shuvaluate "2 * 3")))
  (is (= 7 (shuvaluate "1 + (2 * 3)")))
  (is (= 231 (shuvaluate "1 + 2 * 3 + 4 * 5 + 6")))
  (is (= 51 (shuvaluate "1 + (2 * 3) + (4 * (5 + 6))")))
  (is (= 46 (shuvaluate "2 * 3 + (4 * 5)")))
  (is (= 1445 (shuvaluate "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
  (is (= 669060 (shuvaluate "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
  (is (= 23340 (shuvaluate "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))

(defn part-2 []
  (reduce + (map shuvaluate input)))
