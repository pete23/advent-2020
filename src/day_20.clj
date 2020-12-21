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
    (into edges (reverse edges))))
  
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


(defn part-1 [input]
  ;; edges to tile number
  (let [tiles (mapv parse-tile input)]
    (reduce (fn [edge->tile-number ]
    (mapcat (fn [tile]
              (map (fn [edge] [edge (:tile-number tile)]) (:edges tile))) tiles)))
                      
