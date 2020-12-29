(ns day-19
  (:use clojure.test)
  (:require [clojure.string :refer [split split-lines]]
            [clojure.set :refer [union]]))

(defn atol [s] (Long/parseLong s))

;; number -> rule
;; rule -> production*
;; production -> chained-rule* | literal

;; chained-rule is a number
;; literal is a character


(defn string->chained-rule [s]
  (mapv atol (split s #" ")))

(defn string->production [s]
  (let [literal (re-matches #"\"([ab])\"" s)]
    (if literal
      (conj #{} (second literal)) ;; a literal char matches itself
      (string->chained-rule s))))

(defn string->rule [s]
  (mapv string->production (split s #" \| ")))

(defn string->id-rule [s]
  (let [[rule-number rule] (split s #": " 2)]
    [(atol rule-number) (string->rule rule)]))

(defn lines->rules [lines]
  (into (sorted-map) (map string->id-rule lines)))

(defn compile-productions [])

(defn compile-rule [rules-map rule-id]
  (let [productions (rules-map rule-id)]
    (if (set? productions) rules-map ;; already compiled
        (compile-productions rules-map rule-id productions))))

(defn combine-sets [as bs]
  (into #{} (mapcat (fn [a] (map (fn [b] (str a b)) bs)) as)))

(defn combine [[a b]]
  (if b (combine-sets a b) a))
                     
(defn compile-production [rules-map production]
  (if (set? production) production
      (combine (map rules-map production))))
                          
(defn compile-productions [rules-map rule-id productions]
  ;; first we ensure that all productions are pre-compiled
  ;; this will result in a mutually recursive build of the chain for this rule
  (let [subcompilation (reduce compile-rule rules-map (filter number? (flatten productions)))
        rule (reduce union (map #(compile-production subcompilation %) productions))]
    (assoc subcompilation rule-id rule)))

(defn compile-rules [rules-map]
  (reduce compile-rule rules-map (keys rules-map)))

(defn parse-input [[rules-input check-input]]
  (let [rules (lines->rules (split-lines rules-input))
        check (split-lines check-input)]
    [rules check]))

(defn read-input [file]
  (-> file
      clojure.java.io/resource
      slurp
      (split #"\n\n")
      parse-input))
  
(def input (read-input "day-19.input"))

(def test-rules-1 (->> ["0: 1 2" "1: \"a\"" "2: 1 3 | 3 1" "3: \"b\""]
                       lines->rules))

(defn comparse [lines] (compile-rules (lines->rules lines)))

(deftest test-basic-rules
  (is (= {0 #{"a"} 1 #{"a"}} (comparse ["0: 1" "1: \"a\""])))
  (is (= #{"aa"} ((comparse ["0: 1 1" "1: \"a\""]) 0)))
  (is (= #{"a" "b"} ((comparse ["0: 1" "1: \"a\" | \"b\""]) 0)))
  (is (= #{"ab" "ba" "aa" "bb"} ((comparse ["0: 1 1" "1: \"a\" | \"b\""]) 0))))

(defn part-1 [rules check]
  (count (filter (rules 0) check)))

;; rule zero is 8 then 11
;; rule 8 is 42 n times
;; rule 11 is 42 then 31 or 42 then itself then 31
;; rule zero is then m 42 then n 31 where m>n
;; 31 and 42 both produce strings of length 8
;; (filter (x 31) (x 42)) => ()
;; no common strings produced between them

(defn count-occurrences
  ([s n rules]
   (loop [chunks (map #(apply str %) (partition n s))
          acc []
          current 0
          rules rules]
     (if (or (empty? chunks) (empty? rules)) (conj acc current)
         (let [rule-matches ((first rules) (first chunks))]
           (recur (if rule-matches (rest chunks) chunks)
                  (if rule-matches acc (conj acc current))
                  (if rule-matches (inc current) 0)
                  (if rule-matches rules (rest rules))))))))

(defn new-rule-zero [rules]
  (let [zero-rules [(rules 42) (rules 31)]]
    (fn [s]
      (let [occurrences (count-occurrences s (count (ffirst zero-rules)) zero-rules)]
        (when (= (count occurrences) 2)
          (> (first occurrences) (last occurrences) 0))))))

(defn part-2 [[rules check]]
  (count (filter (new-rule-zero (compile-rules rules)) check)))

(def part-2-test-input (read-input "day-19-2.test-input"))
  
;; let us never speak of this again
;; shoulda just built the regexps lol:-)
