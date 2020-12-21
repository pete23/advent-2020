(ns day-20
  (:require [clojure.string :refer [split split-lines]])
  (:use clojure.test))

(defn atol [s] (Long/parseLong s))

;; 4 edges and reversed
(defn calc-edges [tile]
  (let [edges (vector (first tile)
                      (last tile)
                      (map first tile)
                      (map last tile))]
    (into edges (map reverse edges))))
  
(defn parse-tile [lines]
  (let [[header & tile-lines] (split-lines lines)
        [_ tile-id] (re-matches #"Tile ([0-9]+):" header)
        tile-number (atol tile-id)
        tile (mapv #(mapv {\# 1 \. 0} %) tile-lines)
        edges (calc-edges tile)]
    {:tile-number tile-number
     :tile tile
     :edges edges}))

(def input (-> "resources/day-20.input"
               slurp
               (clojure.string/split #"\n\n")))

(def test-input (-> "resources/day-20.test-input"
                    slurp
                    (clojure.string/split #"\n\n")))

(defn one? [coll] (= 1 (count coll)))

(defn part-1 [input]
  ;; edges to tile number
  (let [tiles (mapv parse-tile input)]
    (reduce (fn [acc tile]
              (reduce #(update %1 %2 conj (:tile-number tile)) acc (:edges tile))) {} tiles)))
              
