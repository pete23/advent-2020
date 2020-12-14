(ns day-13
  (:use clojure.test))

(defn atol [s] (Long/parseLong s))

(def input {:start 1008169
            :buses "29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,37,x,x,x,x,x,653,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,17,x,x,x,x,x,23,x,x,x,x,x,x,x,823,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19"})

(def test-input {:start 939
                 :buses "7,13,x,x,59,x,31,19"})

(defn split-csv [s] (clojure.string/split s #"\,"))

(defn parse-input [input]
  (update input :buses (fn [s] (mapv atol (remove #(= "x" %) (split-csv s))))))

(defn next-bus [{:keys [start buses]}]
  (let [time-until (mapv #(- % (mod start %)) buses)
        [soonest-idx soonest-time]  (apply min-key second (map-indexed vector time-until))
        bus (nth buses soonest-idx)]
    (* bus soonest-time)))

(defn part-1
  ([] (part-1 input))
  ([input] (next-bus (parse-input input))))

(deftest part-1-test
  (is (= 295 (part-1 test-input))))

(defn parse-input-2 [input]
  (remove #(nil? (second %))
          (map-indexed #(vector %1 (when (not= "x" %2) (Long/parseLong %2))) (split-csv (:buses input)))))

(defn brute-part-2 [input]
  (let [tests (reverse (sort-by second (parse-input-2 input)))
        n (second (first input))]
    (loop [i (+ n (first (first input)))]
      (if (every?))))) 
