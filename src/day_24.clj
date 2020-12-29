(ns day-24
  (:require [clojure.string :refer [blank? split-lines]])
  (:use clojure.test))

(def input (->> "resources/day-24.input" slurp split-lines))

(def test-input ["sesenwnenenewseeswwswswwnenewsewsw"
                 "neeenesenwnwwswnenewnwwsewnenwseswesw"
                 "seswneswswsenwwnwse"
                 "nwnwneseeswswnenewneswwnewseswneseene"
                 "swweswneswnenwsewnwneneseenw"
                 "eesenwseswswnenwswnwnwsewwnwsene"
                 "sewnenenenesenwsewnenwwwse"
                 "wenwwweseeeweswwwnwwe"
                 "wsweesenenewnwwnwsenewsenwwsesesenwne"
                 "neeswseenwwswnwswswnw"
                 "nenwswwsewswnenenewsenwsenwnesesenew"
                 "enewnwewneswsewnwswenweswnenwsenwsw"
                 "sweneswneswneneenwnewenewwneswswnese"
                 "swwesenesewenwneswnwwneseswwne"
                 "enesenwswwswneneswsenwnewswseenwsese"
                 "wnwnesenesenenwwnenwsewesewsesesew"
                 "nenewswnwewswnenesenwnesewesw"
                 "eneswnwswnwsenenwnwnwwseeswneewsenese"
                 "neswnwewnwnwseenwseesewsenwsweewe"
                 "wseweeenwnesenwwwswnew"])

(def directions
  {"nw" [0 1 -1]
   "ne" [1 0 -1]
   "w"  [-1 1 0]
   "e"  [1 -1 0]
   "se" [0 -1 1]
   "sw" [-1 0 1]})

(def pattern #"(nw|ne|w|e|se|sw)(.*)?")

(defn add [[x1 y1 z1] [x2 y2 z2]]
  [(+ x1 x2) (+ y1 y2) (+ z1 z2)])

(defn directions->coords
  ([direction-str] (directions->coords [0 0 0] direction-str))
  ([coords direction-str]
   (if (blank? direction-str) coords
       (let [[_ direction remainder] (re-find pattern direction-str)]
         (recur (add coords (directions direction)) remainder)))))

(defn flip [black-tiles coords]
  (if (black-tiles coords)
    (disj black-tiles coords)
    (conj black-tiles coords)))

(defn input->start [input]
  (reduce flip #{} (map directions->coords input)))

(defn part-1 [input]
  (count (input->start input)))

(deftest part-1-test
  (is (= 10 (part-1 test-input)))
  (is (= 512 (part-1 input))))

(defn neighbours [coords]
  (mapv #(add coords %) (vals directions)))

(defn sum-neighbours [black-tiles coords]
  (reduce #(if (black-tiles %2) (inc %1) %1) 0 (neighbours coords)))

(defn run-day [black-tiles]
  (let [interesting (set (mapcat neighbours black-tiles))]
    (persistent!
     (reduce (fn [new-tiles coords]
               (let [n (sum-neighbours black-tiles coords)]
                 (if (or (= 2 n)
                         (and (black-tiles coords) (= 1 n)))
                   (conj! new-tiles coords)
                   new-tiles)))
             (transient #{})
             interesting))))
  
(defn part-2 [input]
  (count
   (nth
    (iterate run-day
             (input->start input))
    100)))
