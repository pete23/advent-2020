(ns day-21
  (:use [clojure.test])
  (:require [clojure.string :refer [replace split split-lines]]
            [clojure.set :refer [difference]]))


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

(defn fix-point [f i]
  (let [next-i (f i)]
    (if (= i next-i)
      i
      (recur f next-i))))

;; this gets super confusing super quickly
;; my apologies to the chef

;; resolved is a map from allergen string to food string
;; unresolved is a map from allergen to a set of possible food strings

;; resolved if there is a single allergen->food mapping
(defn filter-resolved [acc k v]
  (if (= 1 (count v))
    (assoc acc k (first v))
    acc))

;; remove resolved mappings from the unresolved set
;; 1. remove the keys that have been resolved
;; 2. remove the values that have been resolved
(defn remove-resolved [resolved unresolved]
  (let [foods-to-remove (set (vals resolved))
        allergens-to-remove (set (keys resolved))]
    (reduce-kv #(if (allergens-to-remove %2) %1
                    (assoc %1 %2 (difference %3 foods-to-remove))) {} unresolved)))

;; resolver
;; finds any newly resolved mappings (i.e. one food in set)
;; removes those mappings from the unresolved and adds them to the resolved
(defn resolver [[unresolved resolved]]
  (let [newly-resolved (reduce-kv filter-resolved {} unresolved)]
    [(remove-resolved newly-resolved unresolved)
     (into resolved newly-resolved)]))

(defn part-2 [input]
  (let [foods-and-allergens (map parse-line input)
        known-allergens (reduce map-allergens {} foods-and-allergens)
        foods (map second (second (fix-point resolver [known-allergens (sorted-map)])))]
    (apply str (interpose "," foods))))


(deftest day-21-test
  (is (= 5 (part-1 test-input)))
  (is (= 2317 (part-1 input)))
  (is (= "mxmxvkd,sqjhc,fvjkl" (part-2 test-input)))
  (is (= "kbdgs,sqvv,slkfgq,vgnj,brdd,tpd,csfmb,lrnz" (part-2 input))))
