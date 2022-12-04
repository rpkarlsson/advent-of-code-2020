(ns rpkarlsson.2022.day-04
  (:require [clojure.string :as str]
            [rpkarlsson.utils :as u]
            [clojure.set :as set]))


(def sample
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #","))
       (u/mmap #(str/split % #"-"))
       (u/mmap (partial map #(Integer/parseInt %)))
       (mapcat identity)))

(defn overlaps?
  [[a b]]
  (let [a-range (apply range (update-in (vec a) [1] inc))
        b-range (apply range (update-in (vec b) [1] inc))]
    (or
     (set/subset? (set a-range) (set b-range))
     (set/subset? (set b-range) (set a-range)))))

(defn solve-1
  [s]
  (->> s
       parse
       (partition-all 2)
       (map overlaps?)))

#_(->> sample solve-1 (filter true?) count)

#_(->> "resources/2022/day_04.txt" slurp solve-1 (filter true?) count)


(defn any-overlap?
  [[a b]]
  (let [a-range (apply range (update-in (vec a) [1] inc))
        b-range (apply range (update-in (vec b) [1] inc))]
    (seq (set/intersection (set a-range) (set b-range)))))

(defn solve-2
  [s]
  (->> s
       parse
       (partition-all 2)
       (map any-overlap?)))

#_(->> sample solve-2 (remove nil?) count)

#_(->> "resources/2022/day_04.txt" slurp solve-2 (remove nil?) count)
