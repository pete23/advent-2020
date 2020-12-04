(ns day-2
  (:use clojure.test))

(defn atol [s] (Long/parseLong s))

(defn parse-line [l]
  (let [[from to letter password] (clojure.string/split l #"[ \- :]+")]
    {:from (atol from)
     :to (atol to)
     :letter (first letter) ;; str to char
     :password password}))

(defn parse-lines [s]
  (map parse-line (clojure.string/split-lines s)))

(def input (->> "day-2.input"
                clojure.java.io/resource
                slurp
                parse-lines))

(deftest parsing (is
                  (= {:from 2 :to 9 :letter \c :password "ccccccccc"}
                     (parse-line "2-9 c: ccccccccc"))))

(defn check-part-1 [{:keys [password letter from to]}]
  (let [f (frequencies password)]
    (when-let [letter-count (f letter)]
      (<= from letter-count to))))

(def test-input "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")

(deftest part-1-example
  (is (= 2 (count (filter check-part-1 (parse-lines test-input))))))

(defn part-1 []
  (count (filter check-part-1 input)))

(defn check-part-2 [{:keys [password letter from to]}]
  (let [one (nth password (dec from))
        two (nth password (dec to))]
    (not= (= one letter) (= two letter))))

(deftest part-2-example
  (is (= 1 (count (filter check-part-2 (parse-lines test-input))))))

(defn part-2 []
  (count (filter check-part-2 input)))
