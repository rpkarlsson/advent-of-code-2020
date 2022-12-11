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
       (sequence (comp (map #(re-seq #"(\d+)-(\d+)" %))
                       (u/mcatmap (juxt second #(nth % 2)))
                       (u/mmap parse-long)))))

(defn overlaps?
  [[[x1 y1] [x2 y2]]]
  (or (<= x1 x2 y2 y1)
      (<= x2 x1 y1 y2)))

(defn solve-1
  [s]
  (->> (parse s)
       (partition-all 2)
       (map overlaps?)
       (filter true?)
       (count)))

#_(->> sample solve-1)

#_(->> "resources/2022/day_04.txt" slurp solve-1)

(defn any-overlap?
  [[a b]]
  (let [a-range (apply range (update-in (vec a) [1] inc))
        b-range (apply range (update-in (vec b) [1] inc))]
    (seq (set/intersection (set a-range) (set b-range)))))

(defn solve-2
  [s]
  (->> (parse s)
       (partition-all 2)
       (keep any-overlap?)
       (count)))

#_(->> sample solve-2)

#_(->> "resources/2022/day_04.txt" slurp solve-2)
