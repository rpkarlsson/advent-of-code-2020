(ns rpkarlsson.2021.day-14
  (:require
   [rpkarlsson.utils :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def sample
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(def parsed-sample
  (let [[template insertions] (str/split sample #"\n\n")]
    [template (->> (str/split-lines insertions)
                   (map #(str/split % #" -> "))
                   (into {}))]))

(def parsed-input
  (let [[template insertions] (str/split (slurp (io/resource "2021/day_14.txt")) #"\n\n")]
    [template (->> (str/split-lines insertions)
                   (map #(str/split % #" -> "))
                   (into {}))]))

(defn solve
  [input steps]
  (let [[template insertion-rules] input]
    (loop [polymer template
           taken-steps 0]
      (if (<= steps taken-steps)
        polymer
        (let [next-polymer (str (first polymer)
                                (->> polymer
                                     (partition 2 1)
                                     (map (fn [pair]
                                            (apply str (flatten [(get insertion-rules (apply str pair)) (second pair)]))))
                                     (apply str)))]
          (recur next-polymer (inc taken-steps)))))))

(defn part-1
  []
  (let [elements-by-q (->> (solve parsed-input 10)
                           (frequencies)
                           (sort-by second))]
    (- (second (last elements-by-q))
       (second (first elements-by-q)))))
