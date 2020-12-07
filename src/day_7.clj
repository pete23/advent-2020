(ns day-7
  (:use clojure.test)
  (:require [clojure.string :refer [split]]))

(defn atol [s] (Long/parseLong s))

(def input (-> "resources/day-7.input"
                slurp
                (split #"\n")))

(def test-input ["light red bags contain 1 bright white bag, 2 muted yellow bags."
                 "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
                 "bright white bags contain 1 shiny gold bag."
                 "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
                 "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
                 "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
                 "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
                 "faded blue bags contain no other bags."
                 "dotted black bags contain no other bags."])

(defn parse-contained [contained]
  (let [[n colour] (split contained #" " 2)]
    (if (= n "no")
      {}
      {colour (atol n)})))

(deftest test-parse-contained
  (is (= {} (parse-contained "no other"))
      (= {"shiny gold" 2} (parse-contained "2 shiny gold"))))

(defn parse-line [l]
  (let [[container & contained] (split l #"( bags contain | bag(s)?[\,\.] ?)")]
    {container (into {} (map parse-contained contained))}))

(deftest test-parse-line
  (is (= {"dotted black" {}} (parse-line (last test-input)))
      (= {"light red" {"bright white" 1 "muted yellow" 2}} (parse-line (first test-input)))))

(defn parse [lines]
  (into {} (map parse-line lines)))

(defn invert [k->m]
  (let [back-links (mapcat (fn [[k vs]] (map (fn [v] [v k]) (keys vs))) k->m)]
    (reduce #(update %1 (first %2) conj (second %2)) {} back-links)))

(defn containers-for [contained->container colour]
  (let [immediate-containers (contained->container colour)
        recursive-containers (mapcat #(containers-for contained->container %) immediate-containers)]
    ;; great ergonomics - into {} [{}{}] works, although [[][]] doesn't
    (into #{} (concat immediate-containers recursive-containers))))
  
(defn part-1
  ([input] (let [container->contained (parse input)
                 contained->container (invert container->contained)]
             (count (containers-for contained->container "shiny gold")))))

(deftest test-part-1
  (= 4 (part-1 test-input)))

;; yeah, we could do some fancy dynamic programming to build from the bagless roots
;; or we could go for the braindead recursion and then take the dog to the vets:-)
(defn total-bags-inside [container->contained bag]
  (reduce +
          (map #(* (+ 1 (total-bags-inside container->contained (first %))) (second %))
               (container->contained bag))))
  
(defn part-2
  ([input] (total-bags-inside (parse input) "shiny gold")))

(deftest test-part-2
  (= 32 (part-2 test-input)))

