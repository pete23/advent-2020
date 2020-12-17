(ns day-17
  (:require [clojure.string :refer [split split-lines replace]])
  (:use clojure.test)
  (:import [com.carrotsearch.hppc LongByteHashMap LongHashSet]
           [com.carrotsearch.hppc.procedures LongProcedure LongByteProcedure]))

(def test-input [".#." "..#" "###"])
(def input (-> "resources/day-17.input" slurp split-lines))

;; hashes are live cells, dots are empty
(def char->value {\# (byte 1) \. (byte 0)})

;; coordinates encoded as (w+500) * 1000000000 (z+500) * 1000000, (y+500) * 1000, x
(defn coord->key [[^long x ^long y ^long z ^long w]]
  (let [z (or z 0) w (or w 0) y (or y 0) x (or x 0)]
    (+ 500 x
       (* 1000 (+ y 500))
       (* 1000000 (+ z 500))
       (* 1000000000 (+ w 500)))))

(defn key->coord [^long k]
  (let [x (- (rem k 1000) 500)
        k' (quot k 1000)
        y (- (rem k' 1000) 500)
        k'' (quot k' 1000)
        z (- (rem k'' 1000) 500)
        w (- (quot k'' 1000) 500)]
    [x y z w]))

(def dimensional-perms [[-1 0 1] [-1000 0 1000] [-1000000 0 1000000] [-1000000000 0 1000000000]])

(defn neighbours [^long dimensions ^long k]
  (loop [n (vector k) ts (take dimensions dimensional-perms)]
    (if (empty? ts) n
        (recur (mapcat (fn [t] (map #(+ % t) n)) (first ts)) (rest ts)))))

(defn input->coordinate-values [input]
  (apply concat
   (map-indexed
    (fn [y l]
      (map-indexed
       (fn [x c]
         (vector [x y] (char->value c)))
       l))
    input)))

;; Interop to create a LongByteProcedure to sum the memory arena
(defprotocol ValueRetriever
  (get-value [this ^LongByteHashMap matrix]))

(deftype ValueAdder [^{:unsynchronized-mutable true} ^long total]
  LongByteProcedure
  (^void apply [this ^long k ^byte v] (set! total (+ total v)))
  ValueRetriever
  (get-value [this memory] (set! total 0) (.forEach memory this) total))

(deftype RelevantCells [^long dimensions ^LongHashSet relevant]
  LongByteProcedure
  (^void apply [this ^long k ^byte v]
   (reduce #(.add relevant %2) relevant (neighbours dimensions k)))
  ValueRetriever
  (get-value [this matrix] (.forEach matrix this) relevant))

(deftype Compute [^long dimensions ^LongByteHashMap old-matrix ^LongByteHashMap new-matrix]
  LongProcedure
  (^void apply [this ^long k]
   (let [cell (.get old-matrix k)
         sum (reduce + (map #(.get old-matrix %) (neighbours dimensions k)))]
     (cond (and (= cell 1) (or (= sum 3) (= sum 4))) (.put new-matrix k 1)
           (and (= cell 0) (or (= sum 3))) (.put new-matrix k 1)))))
     
(defn cycle [dimensions ^LongByteHashMap matrix]
  (let [relevant-calculator (RelevantCells. dimensions (LongHashSet.))
        ^LongHashSet relevant (.get-value relevant-calculator matrix)
        _ (println "Considering" (.size relevant) "cells based on" (.size matrix) "live cells")
        new-matrix (LongByteHashMap.)
        computer (Compute. dimensions matrix new-matrix)
        _ (.forEach relevant computer)]
    new-matrix))

(defn repeat-cycle [n dimensions matrix]
  (if (= n 0) matrix
      (recur (dec n) dimensions (cycle dimensions matrix))))

(defn input->matrix [input]
  (let [matrix (LongByteHashMap.)]
    (->> input
         input->coordinate-values
         (remove #(= (second %) 0))
         (map first)
         (map coord->key)
         (reduce #(.put matrix %2 (byte 1)) matrix))
    matrix))

(defn create-output [^LongByteHashMap matrix]
  (.size matrix))

(defn evaluate [dimensions input cycles]
  (->> input
       input->matrix
       (repeat-cycle cycles dimensions)
       create-output))

(deftest part-1-test
  (is (= 112 (evaluate 3 test-input 6))
      (= 276 (evaluate 3 input 6))))

(deftest part-2-test
  (is (= 848 (evaluate 4 test-input 6))
      (= 2136 (evaluate 4 input 6))))
