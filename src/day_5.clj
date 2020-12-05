(ns day-5
  (:use clojure.test)
  (:require [clojure.string]))

(defn parse-pass [s]
  (-> s
      (clojure.string/replace #"[BR]" "1")
      (clojure.string/replace #"[FL]" "0")
      (Long/parseLong 2)))

(defn id->row [^long n]
  (bit-shift-right n 3))

(defn id->column [^long n]
  (bit-and n 7))

(defn test-pass [p]
  (let [id (parse-pass p)]
    [id (id->row id) (id->column id)]))

(deftest parse-pass-test
  (are [pass expected]
      (= (test-pass pass) expected)
    "FBFBBFFRLR" [357 44 5]
    "BFFFBBFRRR" [567 70 7]
    "FFFBBBFRRR" [119 14 7]
    "BBFFBBFRLL" [820 102 4]))

(def input (->> "resources/day-5.input"
;                clojure.java.io/resource
                slurp
                clojure.string/split-lines
                (mapv parse-pass)
                (into (sorted-set))))

(defn part-1 []
  (last input))

(defn part-2
  ([] (loop [^long n (first input)]
        (if (not (input n))
          n
          (recur (inc n))))))

        
    
