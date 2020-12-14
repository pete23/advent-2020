(ns day-14
  (:require [clojure.string :refer [split split-lines replace]])
  (:use clojure.test))

(def test-input ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                 "mem[8] = 11"
                 "mem[7] = 101"
                 "mem[8] = 0"])

(def input (-> "resources/day-14.input" slurp split-lines))

(defn atol [s] (Long/parseLong s))

(defn btol [b-str] (Long/parseLong b-str 2))

(defn parse-mask [mask x]
  (btol (replace mask "X" (str x))))

;; this mask fn changes the computer's assignment fn to mask the value written
(defn set-mask [computer mask-str]
  (let [and-mask (parse-mask mask-str 1)
        or-mask (parse-mask mask-str 0)
        mask-value-fn (fn [val] (-> val (bit-and and-mask) (bit-or or-mask)))]
    (assoc computer
           :assignment-fn
           (fn [computer address value]
             (assoc-in computer [:memory address] (mask-value-fn value))))))

(defn mask [[mask-str]]
  (fn [computer]
    ((:mask-fn computer) computer mask-str)))

(defn assignment [[address-str value-str]]
  (fn [computer]
    ((:assignment-fn computer) computer (atol address-str) (atol value-str))))

;; instructions are defined as a map of
;; matching regexp -> instruction creation fn to be called with match groups
(def instructions
  {#"mem\[([0-9]+)\] = ([0-9]+)" assignment
   #"mask = ([01X]+)" mask})

(defn try-parse-instruction [s [re f]]
  (when-let [[_ & groups] (re-matches re s)]
    (f groups)))

(defn parse-instruction [s]
  (some (partial try-parse-instruction s) instructions))

(deftest check-instruction-parse
  (let [i (parse-instruction "mem[23] = 666")]
    (is (= {:memory {23 666} :mask-fn identity} (i {:mask-fn identity}))
        (= {:memory {23 -666} :mask-fn -} (i {:mask-fn -}))))
  (let [i (parse-instruction "mask = 111")]
    (is (= 7 ((:mask-fn (i {})) 0)))))

(defn lines->computer [mask-fn lines]
  {:memory {}
   :instructions (mapv parse-instruction lines)
   :mask-fn mask-fn})

(defn run-program [computer]
  (reduce (fn [c i] (i c)) computer (:instructions computer)))

(defn create-output [computer]
  (reduce + (vals (:memory computer))))

(defn part-1
  ([] (part-1 input))
  ([input] (->> input
                (lines->computer set-mask)
                run-program
                create-output)))

(deftest part-1-test
  (is (= 165 (part-1 test-input))))

(defn bits-set
  ([l] (bits-set l [] 0))
  ([l s c] (let [s (if (= 1 (bit-and 1 l)) (conj s c) s)
                 l (bit-shift-right l 1)]
             (if (= l 0) s
                 (recur l s (inc c))))))

(defn set-bits
  ([indexes-of-bits bits] (set-bits indexes-of-bits bits 0))
  ([indexes-of-bits bits acc]
   (if (empty? bits) acc
       (set-bits indexes-of-bits (rest bits) (bit-set acc (indexes-of-bits (first bits)))))))
   
(defn bit-permutations
  ([l] (let [our-bits (bits-set l)
             how-many-bits (count our-bits)]
         (map #(set-bits our-bits (bits-set %)) (range (Math/pow 2 how-many-bits))))))

(deftest bit-permutations-test
  (is (= [0] (bit-permutations 0))
      (= [0 2 8 10] (bit-permutations 10))))
             
(defn set-quantum-mask [computer mask-str]
  (let [or-mask (parse-mask mask-str 1)
        open-bits (btol (replace mask-str #"[1X]" {"1" "0" "X" "1"}))
        open-permutations (bit-permutations open-bits)
        open-mask (bit-not open-bits)]
    (assoc computer
           :assignment-fn
           (fn [computer address value]
             (let [addresses (map #(-> address
                                       (bit-or or-mask)    ; 1 set to 1
                                       (bit-and open-mask) ; open bits set to 0
                                       (bit-or %))         ; set bits as permuted
                                  open-permutations)
                   assignments (zipmap addresses (repeat value))]
             (update-in computer [:memory] merge assignments))))))

(defn part-2
  ([] (part-2 input))
  ([input] (->> input
                (lines->computer set-quantum-mask)
                run-program
                create-output)))

;; i note different test input is given for part 2:-)
;; it would be possible to deal with the test input for part 1 but you'd need
;; a different machine that didn't realize the whole memory space but instead dealt
;; with superpositional overlays that weren't reified to provide the sum
;; i.e. set mask XX, mem[_] = 100, sum += 2^4 * 100
(def part-2-test-input ["mask = 000000000000000000000000000000X1001X"
                        "mem[42] = 100"
                        "mask = 00000000000000000000000000000000X0XX"
                        "mem[26] = 1"])

;; i'm going to leave the above as an EEIR:-)
(deftest part-2-test
  (is (= 208 (part-2 part-2-test-input))))
                       
