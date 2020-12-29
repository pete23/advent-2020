(ns day-23
  (:use [clojure.test])
  (:import [com.carrotsearch.hppc IntIntHashMap]))


(def test-input (parse-input "389125467"))
(def input (parse-input "583976241"))

(defn parse-input [s]
  (mapv #(- (long %) (long \0)) s))

(defn find-dest [^IntIntHashMap ring curr x y z]


;; (curr) -> [x -> y -> z] -> a -> b -> c -> dest -> e
;; curr -> (a) -> b -> c -> dest -> x -> y -> z -> e
(defn crab-2 [^IntIntHashMap ring ^long curr]
  (let [x (.get ring curr)
        y (.get ring x)
        z (.get ring y)
        a (.get ring z)
        _ (.put ring curr a)
        dest (loop [curr curr]
               (let [next-dest (if (= curr 1) (.size ring) (dec curr))]
                 (if (and (not= next-dest x) (not= next-dest y) (not= next-dest z)) next-dest
                     (recur next-dest))))
        e (.get ring dest)
        _ (.put ring dest x)
        _ (.put ring x y)
        _ (.put ring y z)
        _ (.put ring z e)]
    a))

(defn crab-n-2 [n ring curr]
  (if (= n 0) ring
      (recur (dec n) ring (crab-2 ring curr))))

(defn result-2 [^IntIntHashMap ring]
  (* (.get ring 1) (.get ring (.get ring 1))))


(defn make-ring [s]
  (let [ring (int-array (inc (count s)))]
    (areduce #(do (.put ring %1 %2) %2) (last s) s)
    ring))

(defn answer-1 [ring]
  (loop [curr 1 a []]
    (let [next (.get ring curr)]
      (if (= 1 next) a
          (recur next (conj a next))))))

(defn part-1 [input]
  (let [ring (make-ring input)]
    (answer-1 (crab-n-2 100 ring (first input)))))

(defn part-2 [input]
  (let [ring (make-ring (concat input (range 10 1000001)))]
    (result-2 (crab-n-2 10000000 ring (first input)))))
    

(deftest part-1-test
  (is (= (parse-input "92658374") (result (crab-n test-input 10))))
  (is (= (parse-input "67384529") (part-1 test-input))))


;; can't recopy the arrays everywhere now
;; maybe a linked list? then we have
;; need to find dest though?
