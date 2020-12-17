(ns day-17
  (:require [clojure.string :refer [split-lines]])
  (:use clojure.test)
  (:import [com.carrotsearch.hppc LongByteHashMap LongHashSet]
           [com.carrotsearch.hppc.procedures LongProcedure LongByteProcedure]))

(def test-input [".#." "..#" "###"])
(def input (-> "resources/day-17.input" slurp split-lines))

;; hashes are live cells, dots are empty cells
(def char->value {\# true \. false})

(defn input->coordinate-values [input]
  (apply concat
   (map-indexed
    (fn [y l]
      (map-indexed
       (fn [x c]
         (vector [x y] (char->value c)))
       l))
    input)))

;; each dimension is encoded +500 * 100^d
;; HOWEVER this doesn't need to be multidimensional here as the only conversion
;; done is from x,y into the encoding
(defn coord->key [[^long x ^long y]]
  (+ 500 x
     (* 1000 (+ y 500))))

(defn input->matrix [input]
  (let [matrix (LongByteHashMap.)]
    (->> input
         input->coordinate-values
         (filter second)
         (map first)
         (map coord->key)
         (reduce #(.put matrix %2 (byte 1)) matrix))
    matrix))

(defn neighbour-transforms [dimensions]
  (let [magnitude (long (Math/pow 1000 (dec dimensions)))]
    (if (= dimensions 0) [0]
        (vec (mapcat (fn [direction]
                       (map #(+ % (* direction magnitude))
                            (neighbour-transforms (dec dimensions)))) [-1 0 1])))))

(defn ->neighbour-fn [dimensions]
  (let [v (neighbour-transforms dimensions)]
    (fn [k] (mapv #(+ k %) v))))

;; Interop to create a LongByteProcedure to dig in the matrix
(defprotocol ValueRetriever
  (get-value [this ^LongByteHashMap matrix]))

(deftype RelevantCells [neighbour-fn ^LongHashSet relevant]
  LongByteProcedure
  (^void apply [this ^long k ^byte _]
   (reduce #(.add relevant %2) relevant (neighbour-fn k)))
  ValueRetriever
  (get-value [this matrix] (.forEach matrix this) relevant))

(deftype Compute [neighbour-fn ^LongByteHashMap old-matrix ^LongByteHashMap new-matrix]
  LongProcedure
  (^void apply [this ^long k]
   (let [cell (.get old-matrix k)
         sum (reduce + (map #(.get old-matrix %) (neighbour-fn k)))]
     (when (or (= sum 3)
               (and (= cell 1) (= sum 4)))
       (.put new-matrix k 1)))))
     
(defn run-cycle [neighbour-fn ^LongByteHashMap matrix]
  (let [relevant-calculator (RelevantCells. neighbour-fn (LongHashSet.))
        ^LongHashSet relevant (.get-value relevant-calculator matrix)
        ;_ (println "Considering" (.size relevant) "cells based on" (.size matrix) "live cells")
        new-matrix (LongByteHashMap.)
        computer (Compute. neighbour-fn matrix new-matrix)
        _ (.forEach relevant computer)]
    new-matrix))

(defn repeat-cycle [neighbour-fn n matrix]
  (if (= n 0) matrix
      (recur neighbour-fn (dec n) (run-cycle neighbour-fn matrix))))

(defn create-output [^LongByteHashMap matrix]
  (.size matrix))

(defn evaluate [dimensions input cycles]
  (let [neighbour-fn (->neighbour-fn dimensions)]
    (->> input
         input->matrix
         (repeat-cycle neighbour-fn cycles)
         create-output)))

(deftest part-1-test
  (is (= 112 (evaluate 3 test-input 6))
      (= 276 (evaluate 3 input 6))))

(deftest part-2-test
  (is (= 848 (evaluate 4 test-input 6))
      (= 2136 (evaluate 4 input 6))))
