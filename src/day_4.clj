(ns day-4
  (:use clojure.test)
  (:require [clojure.string :refer [split]]))

(defn parse-record [s]
  (reduce #(let [[k v] (split %2 #":")]
             (assoc %1 (keyword k) v))
          {}
          (split s #"[ \n]")))

(deftest parse-record-test
  (is (= {:ecl "gry" :iyr "2017" :cid "147"}
         (parse-record "ecl:gry iyr:2017\ncid:147"))))
    
(defn parse-input [s]
  (mapv parse-record (split s #"\n\n")))

(def input (->> "day-4.input"
                clojure.java.io/resource
                slurp
                parse-input))

(def test-input (parse-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"))

(def required-keys [:byr :iyr :eyr :hgt :hcl :ecl :pid])
(defn valid-passport? [p]
  (every? p required-keys))

(deftest part-1-example
  (is (= 2 (count (filter valid-passport? test-input)))))

(defn part-1 []
  (count (filter valid-passport? input)))

(defn atol [s] (Long/parseLong s))

(defn height-validator [s]
  (let [[_ number unit] (re-matches #"([0-9]{2,3})(cm|in)" s)]
    (cond
      (= unit "cm") (<= 150 (atol number) 193)
      (= unit "in") (<= 59 (atol number) 76))))

(defn regex-validator [r]
  (fn [s] (re-matches r s)))

(def field-validators
  {:byr #(<= 1920 (atol %) 2002)
   :iyr #(<= 2010 (atol %) 2020)
   :eyr #(<= 2020 (atol %) 2030)
   :hgt height-validator
   :hcl (regex-validator #"\#[0-9a-f]{6}")
   :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   :pid (regex-validator #"[0-9]{9}")})

(defn valid-passport-fields? [p]
  (and (valid-passport? p)
       (every? (fn [[field valid?]]
                 (valid? (p field))) field-validators)))

(def valid-passports (parse-input "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"))

(def invalid-passports (parse-input "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"))

(deftest test-field-validation
  (is (= (count valid-passports) (count (filter valid-passport-fields? valid-passports)))
      (= 0 (count (filter valid-passport-fields? invalid-passports)))))

(defn part-2 []
  (count (filter valid-passport-fields? input)))
