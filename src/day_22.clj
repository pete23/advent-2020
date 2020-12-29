(ns day-22
  (:use [clojure.test])
  (:require [clojure.data.finger-tree :refer [double-list]]
            [clojure.string :refer [replace split split-lines starts-with?]]
            [clojure.set :refer [difference]]))


(def test-input [["Player 1:" "9" "2" "6" "3" "1"]
                 ["Player 2:" "5" "8" "4" "7" "10"]])

(def input (mapv split-lines (split (slurp "resources/day-22.input") #"\n\n")))

(defn atol [a] (Long/parseLong a))

(defn parse-hand [hand]
  (mapv atol (rest hand)))

(defn parse-input [input]
  (map parse-hand input))

(defn play-round [[[p1-card & p1-hand] [p2-card & p2-hand]]]
  (if (> p1-card p2-card)
    [(conj p1-hand p1-card p2-card) p2-hand]
    [p1-hand (conj p2-hand p2-card p1-card)]))

(defn play-rounds [hands]
  (if (some empty? hands) hands
      (recur (play-round hands))))

(deftest part-1-test
  (is (= [nil '(3 2 10 6 8 5 9 4 7 1)] (play-rounds (parse-input test-input)))))

(defn score-winner [hands]
  (let [winning-hand (first (remove empty? hands))]
    (reduce + (map #(* %1 %2)
                   winning-hand
                   (range (count winning-hand) 0 -1)))))

(defn part-1 [input]
  (let [hands (play-rounds (parse-input input))]
    (score-winner hands)))

(def play-recursive-rounds [hands])

(defn play-recursive-round [[p1 p2]]
  (let [p1-card (first p1) p1-hand (subvec p1 1)
        p2-card (first p2) p2-hand (subvec p2 1)]
    (let [p1-win (if (and (<= p1-card (count p1-hand))
                          (<= p2-card (count p2-hand)))
                   (not
                    (empty?
                     (first
                      (play-recursive-rounds [(subvec p1-hand 0 p1-card)
                                              (subvec p2-hand 0 p2-card)]))))
                   (> p1-card p2-card))]
      (if p1-win
        [(conj p1-hand p1-card p2-card) p2-hand]
        [p1-hand (conj p2-hand p2-card p1-card)]))))

(defn play-recursive-rounds
  ([hands] (play-recursive-rounds #{} hands))
  ([states hands]
   (if (or (states hands)       ;; seen this before, game over
           (some empty? hands)) ;; if there's an empty hand, game over
     hands
     (recur (conj states hands)
            (play-recursive-round hands)))))

(defn part-2 [input]
  (let [hands (parse-input input)
        winning-hands (play-recursive-rounds hands)]
    (score-winner winning-hands)))

(deftest part-2-test
  (is (= 291 (part-2 test-input)))
  (is (= 32054 (part-2 input))))
