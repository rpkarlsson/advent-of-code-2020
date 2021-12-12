(ns rpkarlsson.2020.day-04
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [criterium.core :as c]
   [rpkarlsson.util.file :as file]))

(def code->kw
  {"byr" ::birth-year
   "iyr" ::issue-year
   "eyr" ::expiration-year
   "hgt" ::height
   "hcl" ::hair-color
   "ecl" ::eye-color
   "pid" ::passport-id
   "cid" ::country-id})

(defn process-line
  [line]
  (->> (str/split line #" |\n")
       (into {} (map (fn [k+v]
                       (let [[k v] (str/split k+v #":")]
                         [(code->kw k) v]))))))

(def input-xform
  (comp (partition-by str/blank?)
        (remove (comp str/blank? first))
        (map #(str/join " " %))
        (map process-line)))

(defn validate-keyset
  [passport]
  (let [expected (into #{} (vals (dissoc code->kw "cid")))]
    (set/subset? expected (into #{} (keys (dissoc passport "cid"))))))

(defn part-1
  []
  (->> (file/xf-day-lines 4 input-xform)
       (map validate-keyset)
       (filter true?)
       (count)))

(s/def ::birth-year #(<= 1920 (Integer/parseInt %) 2002))
(s/def ::issue-year #(<= 2010 (Integer/parseInt %) 2020))
(s/def ::expiration-year #(<= 2020 (Integer/parseInt %) 2030))
(s/def ::height
  (s/and string?
         (s/or :cm (s/and #(re-matches #"\d+cm" %)
                          #(let [height (Integer/parseInt (re-find #"\d+" %))]
                             (<= 150 height 193)) )
               :in (s/and #(re-matches #"\d+in" %)
                          #(let [height (Integer/parseInt (re-find #"\d+" %))]
                             (<= 59 height 76)) ))))
(s/def ::hair-color #(re-matches #"^#(\d|[a-f]){6}" %))
(s/def ::eye-color #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::passport-id #(re-matches #"\d{9}" %))

(s/def ::passport
  (s/keys
   :req [::birth-year
         ::issue-year
         ::expiration-year
         ::height
         ::hair-color
         ::eye-color
         ::passport-id]))

(defn part-2
  []
  (->> (file/xf-day-lines 4 input-xform)
       (map #(s/valid? ::passport %))
       (filter true?)
       (count)))

(comment
  (c/bench (part-1))
  (c/bench (part-2)))
