(ns rpkarlsson.day-02
  (:require
   [clojure.string :as str]
   [criterium.core :as c]
   [rpkarlsson.util.file :as file]))

(defn parse-input
  [s]
  (let [[range letter password] (str/split s #" ")]
    [(->> (str/split range #"-")
          (map #(Integer/parseInt %)))
     (subs letter 0 1)
     password]))

(defn policy
  [[lower-limit higher-limit] letter password]
  (let [frequency (count (re-seq (re-pattern letter) password))]
    (<= lower-limit frequency higher-limit)))

(def input (delay (file/xf-day-lines 2 (map parse-input))))

(defn part-1
  []
  (->> @input
       (map #(apply policy %))
       (remove false?)
       (count)))

(defn policy-2
  [[pos-a pos-b] letter password]
  (let [b (= (first letter) (nth password (- pos-b 1)))]
    (if (= (first letter) (nth password (- pos-a 1)))
      (not b)
      b)))

(defn part-2
  []
  (->> @input
       (map #(apply policy-2 %))
       (remove false?)
       (count)))

(comment
  (c/bench (part-1))
  (c/bench (part-2)))
