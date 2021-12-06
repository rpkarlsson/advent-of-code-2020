(ns rpkarlsson.2021.day-01
  (:require [clojure.java.io :as io]))

(def sample
  [199 200 208 210 200 207 240 269 260 263])

(def input
  (with-open [f (io/reader (io/resource "2021/day_01.txt"))]
    (->> (line-seq f)
         (map #(Integer/parseInt %))
         (into []))))

(defn part-1
  []
  (->> input
       (partition-all 2 1)
       (filter #(apply < %))
       (rest)
       (count)))

(defn part-2
  []
  (->> input
       (partition-all 3 1)
       (map #(reduce + %))
       (partition-all 2 1)
       (filter #(apply < %))
       (rest)
       (count)))

(comment
  (part-1)

  (part-2)

  ,)
