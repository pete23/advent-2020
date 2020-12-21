(ns day-16
  (:use [clojure.test])
  (:require [clojure.string :refer [replace split split-lines starts-with?]]
            [clojure.set :refer [difference]]))


(def test-input [["class: 1-3 or 5-7"
                  "row: 6-11 or 33-44"
                  "seat: 13-40 or 45-50"]
                 ["your ticket:" "7,1,14"]
                 ["nearby tickets:"
                  "7,3,47"
                  "40,4,50"
                  "55,2,20"
                  "38,6,12"]])

(def input (map split-lines (split (slurp "resources/day-16.input") #"\n\n")))

(defn atol [a] (Long/parseLong a))

(defn parse-range [line]
  (let [[_ field-name from1 to1 from2 to2] (re-matches #"([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" line)]
    [field-name (into #{} (concat (range (atol from1) (inc (atol to1)))
                                  (range (atol from2) (inc (atol to2)))))]))

(defn parse-ticket [line]
  (mapv atol (split line #",")))

(defn part-1 [input]
  (let [ranges (mapv parse-range (first input))
        tickets (mapv parse-ticket (rest (nth input 2)))
        all-valid (reduce into #{} (map second ranges))
        all-values (reduce into [] tickets)]
    
    (reduce + (remove all-valid all-values))))

(defn check-fields-1 [fields ticket]
  (into {} (filter (fn [[_ valid]] (valid ticket)) fields)))

(defn check-fields [acc ticket]
  (mapv check-fields-1 acc ticket))

(defn fix-point [f i]
  (let [next-i (f i)]
    (if (= i next-i) i (recur f next-i))))

(defn resolver [fields]
  (let [resolved-fields (into #{} (map first (filter #(= (count %) 1) fields)))]
    (map #(if (> (count %) 1) (remove resolved-fields %) %) fields)))

(defn part-2 [input]
  (let [ranges (mapv parse-range (first input))
        my-ticket (parse-ticket (second (second input)))
        tickets (mapv parse-ticket (rest (nth input 2)))
        all-valid (reduce into #{} (map second ranges))
        ;; validate tickets
        tickets (filter #(empty? (remove all-valid %)) tickets)
        fields (into {} ranges)
        ordered-fields (map first
                            (fix-point resolver
                                       (map keys
                                            (reduce check-fields
                                                    (repeat (count fields) fields)
                                                    tickets))))]
    (reduce *
            (map second
                 (filter #(starts-with? (first %) "departure")
                         (zipmap ordered-fields my-ticket))))))
