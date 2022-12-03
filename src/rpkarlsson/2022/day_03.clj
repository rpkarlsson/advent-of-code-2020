(ns rpkarlsson.2022.day-03
  (:require
   [rpkarlsson.utils :as u]
   [clojure.string :as str]
   [clojure.set :as set]))

(def sample
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn find-intersection
  [s]
  (->> s
       (split-at (/ (count s) 2))
       (map set)
       (apply set/intersection)))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn char->priority
  [char]
  (let [char-lower (first (str/lower-case char))
        value (+ 1 (str/index-of alphabet char-lower))]
    (if (= char char-lower)
      value
      (+ value 26))))

(defn solve
  [s]
  (->> s
       (str/split-lines)
       (map find-intersection)
       (u/mmap char->priority)
       (mapcat identity)
       (reduce +)))

#_(solve sample)

(defn part-1
  []
  (->> "resources/2022/day_03.txt" slurp solve))

(defn solve-2
  [s]
  (->> s
       (str/split-lines)
       (partition 3)
       (u/mmap set)
       (map (partial apply set/intersection))
       (u/mmap char->priority)
       (mapcat identity)
       (reduce +)))

#_(solve-2 sample)

(defn part-2
  []
  (->> "resources/2022/day_03.txt" slurp solve-2))
