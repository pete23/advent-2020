(ns day-6
  (:use clojure.test)
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [union intersection]]))

(def input (-> "resources/day-6.input"
                slurp
                (split #"\n\n")))

(defn count-group [f s]
  (count (apply f (map #(apply hash-set %) (split s #"\n")))))

(defn calculate [f input]
  (reduce + (map (partial count-group f) input)))

(defn part-1 [input]
  (calculate union input))

(def test-input ["abc" "a\nb\nc" "ab\nac" "a\na\na\na" "b"])

(deftest part-1-test
  (is (= 11 (part-1 test-input))))

(defn part-2 [input]
  (calculate intersection input))

(deftest part-2-test
  (is (= 6 (part-2 test-input))))
