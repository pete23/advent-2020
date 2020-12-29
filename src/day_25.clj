(ns day-25
  (:require [clojure.string :refer [blank? split-lines]])
  (:use clojure.test))

(defn atol [a] (Long/parseLong a))

(def input [13316116 13651422])
(def test-input [5764801 17807724])

(defn transform ^long [^long subject ^long n]
  (rem (* subject n) 20201227))

(defn run-loop ^long
  ([^long subject ^long loop-size] (run-loop subject 1 loop-size))
  ([^long subject ^long n ^long loop-size]
   (if (= loop-size 0) n
       (recur subject (transform subject n) (dec loop-size)))))

(defn find-loop-size
  ([^long k] (find-loop-size k 0 1))
  ([^long k ^long loop-size ^long n]
   (if (= k n)
     loop-size
     (recur k (inc loop-size) (transform 7 n)))))

(defn part-1 [input]
  (let [[door-pub key-pub] input
        [door-loop key-loop] (mapv find-loop-size input)]
    [(run-loop door-pub key-loop) (run-loop key-pub door-loop)]))
    
    
