(ns day-14
  (:require [clojure.string :refer [split split-lines replace]]
            [clj-java-decompiler.core :refer [decompile]]
            [criterium.core :refer [bench quick-bench]])
  (:use clojure.test)
  (:import [com.carrotsearch.hppc LongLongHashMap]
           [com.carrotsearch.hppc.procedures LongLongProcedure]
           [advent VoidHelper]))

(set! *unchecked-math* true)

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

(defn lines->computer [lines mask-fn]
  {:memory {}
   :instructions (mapv parse-instruction lines)
   :mask-fn mask-fn})

(defn run-program [computer]
  (reduce (fn [c i] (i c)) computer (:instructions computer)))

(defn create-output [computer]
  (reduce + (vals (:memory computer))))

(defn part-1
  ([] (part-1 input))
  ([input] (-> input
               (lines->computer set-mask)
               run-program
               create-output)))

(deftest part-1-test
  (is (= 165 (part-1 test-input))))

(defn bits-set ^bytes [^long x]
  "Return which bits are set in a given long"
  (let [len (Long/bitCount x)]
   (loop [l x s (byte-array len) i 0 b 0]
     (if (= i len) s
         (if (= 1 (bit-and 1 l))
           (do (aset s i (byte b))
               (recur (bit-shift-right l 1) s (inc i) (inc b)))
           (recur (bit-shift-right l 1) s i (inc b)))))))

(defn set-bits ^long [^bytes indexes-of-bits ^bytes bits]
  "Return a long based on setting bits based on an index of bits"
  (areduce bits i ret (long 0)
           (bit-or ret (bit-shift-left 1 (aget ^bytes indexes-of-bits (aget ^bytes bits i))))))

(defn bit-permutations
  "Return an array of longs which are all the possible permutations of the bits in the input long."
  ([l] 
   (let [^bytes our-bits (bits-set l)
         how-many-bits (alength our-bits)
         n-permutations (Math/pow 2 how-many-bits)
         ^longs permutations (long-array n-permutations)]
     (areduce ^longs permutations i ret (long 0)
              (long
               (aset ^longs permutations i (set-bits our-bits (bits-set i)))))
     permutations)))

(deftest bit-permutations-test
  (is (= [0] (into [] (bit-permutations 0)))
      (= [0 2 8 10] (into [] (bit-permutations 10)))))

;; I jokingly call this set-quantum-mask as the bits are in an indeterminate state
;; That's not what quantum means! Please read Quantum Computing Since Democritus by
;; Scott Aaronson before engaging in any discussions.
(defn set-quantum-mask [assigner computer mask-str]
  (let [set-mask (parse-mask mask-str 1)
        open-bits (btol (replace mask-str #"[1X]" {"1" "0" "X" "1"}))
        open-permutations (bit-permutations open-bits)
        open-mask (bit-not open-bits)]
    (assoc computer
           :assignment-fn
           (fn [computer address value]
             (let [masked-address (-> address (bit-or set-mask) (bit-and open-mask))
                   addresses (amap ^longs open-permutations idx ret (bit-or masked-address (aget ^longs open-permutations idx)))]
               (update computer :memory assigner addresses value))))))

(defn turbo-assign [^LongLongHashMap memory ^longs addresses ^long value]
  ;(Fast/turboAssign memory addresses value)
  (areduce ^longs addresses i ret 0 (.put memory (aget addresses i) value))
  memory)

;; Interop to create a LongLongProcedure to sum the memory arena
(defprotocol ValueRetriever
  (get-value [this ^LongLongHashMap memory]))

(deftype ValueAdder [^{:unsynchronized-mutable true} ^long total]
  LongLongProcedure
  (^void apply [this ^long k ^long v]
   ;; this gets boxed unnecessarily - thread at https://groups.google.com/g/clojure/c/sqFxKoTt1tQ
   (set! total (unchecked-add total v))
   nil)
  ValueRetriever
  (get-value [this memory] (set! total 0) (.forEach memory this) total))

(defn computer->value [computer]
  (let [v (ValueAdder. 0)]
    (.get-value v (:memory computer))))
    
(defn part-2
  ([] (part-2 input))
  ([input] (-> input
               (lines->computer (partial set-quantum-mask turbo-assign))
               (assoc :memory (LongLongHashMap. 250000))
               run-program
               computer->value)))
  
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
