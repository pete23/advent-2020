(ns day-1
  (:require [clojure.math.combinatorics :as combo])
  (:use clojure.test))

(defn atol [s] (Long/parseLong s))

(def input (->> "day-1.input"
                clojure.java.io/resource
                slurp
                clojure.string/split-lines
                (map atol)
                (into #{})))

(def test-input #{1721 979 366 299 675 1456})

(defn find-n-that-add-to-x [n x s]
  (some #(if (= (reduce + %) x) %) (combo/combinations s n)))

(defn day-1
  ([n] (day-1 n input))
  ([n input] (reduce * (find-n-that-add-to-x n 2020 input))))

(defn day-1-1 []
  (day-1 2))

(defn day-1-2 []
  (day-1 3))

(deftest test-day-1-1
  (is (= (day-1 2 test-input) 514579)))

(deftest test-day-1-2
  (is (= (day-1 3 test-input) 241861950))) 
