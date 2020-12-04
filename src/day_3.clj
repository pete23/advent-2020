(ns day-3
  (:use clojure.test))

(defn parse-input [i]
  (let [board (mapv #(mapv (fn [c] (= \# c)) %) i)
        width (count (first board))
        depth (count board)]
    {:board board :width width :depth depth}))

(def input (->> "day-3.input"
                clojure.java.io/resource
                slurp
                clojure.string/split-lines
                parse-input))

(def test-input ["..##......."
                 "#...#...#.."
                 ".#....#..#."
                 "..#.#...#.#"
                 ".#...##..#."
                 "..#.##....."
                 ".#.#.#....#"
                 ".#........#"
                 "#.##...#..."
                 "#...##....#"
                 ".#..#...#.#"])

(deftest parsing (let [{:keys [board width]} (parse-input test-input)]
                   (is (= 11 width)
                       (= [false true false false true false false false true false true] (last board)))))

(defn count-trees
  ([input step-x step-y] (count-trees input step-x step-y 0 0 0))
  ([{:keys [board width depth] :as input} step-x step-y x y n-trees]
   (if (>= y depth)
     n-trees
     (recur input step-x step-y
            (mod (+ x step-x) width)
            (+ y step-y)
            (if ((board y) x) (inc n-trees) n-trees)))))

(deftest part-1-example
  (is (= 7 (count-trees (parse-input test-input) 3 1))))

(defn part-1 []
  (count-trees input 3 1))

(def steps [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn part-2
  ([] (part-2 input steps))
  ([input steps]
   (reduce * (map
              (fn [[step-x step-y]]
                (count-trees input step-x step-y))
              steps))))

(deftest part-2-example
  (is (= 336 (part-2 (parse-input test-input) steps))))

