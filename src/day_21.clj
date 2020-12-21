(ns day-21
  (:use [clojure.test])
  (:require [clojure.string :refer [replace split split-lines]]))


(def test-input ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
                 "trh fvjkl sbzzf mxmxvkd (contains dairy)"
                 "sqjhc fvjkl (contains soy)"
                 "sqjhc mxmxvkd sbzzf (contains fish)"])

(def input (split-lines (slurp "resources/day-21.input")))

(defn parse-line [line]
  (let [[food-str allergen-str] (split line #" \(contains ")
        foods (into #{} (split food-str #" "))
        allergens (split (replace allergen-str #"[\,\)]" "") #" ")]
    {:foods foods :allergens allergens}))

for each allergen
    not in map? assoc allergen foods
in map? update allergen intersection foods

(defn map-allergen [foods acc allergen]
  (if (contains? acc allergen)
    (update acc allergen clojure.set/intersection foods)
    (assoc acc allergen foods)))

(defn map-allergens [acc {:keys [foods allergens]}]
  (reduce (partial map-allergen foods) acc allergens))

(defn part-1 [input]
  (let [foods-and-allergens (map parse-line input)
        all-foods (mapcat :foods foods-and-allergens)
        known-allergens (reduce map-allergens {} foods-and-allergens)
        problem-foods (reduce clojure.set/union (vals known-allergens))]
    (count (remove problem-foods all-foods))))
