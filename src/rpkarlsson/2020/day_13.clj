(ns rpkarlsson.2020.day-13
  (:require [clojure.string :as str]
            [rpkarlsson.util.file :as file]))

(defn closest-to-target
  [target sum next]
  (if (<= target sum)
    (reduced sum)
    (+ sum next)))

(defn id+wait+departure
  [interval target]
  (let [departure (reduce (partial closest-to-target target) 0 (repeat interval))]
    [interval (- departure target) departure]))

(defn part-1
  []
  (let [target 1005595]
    (->> (file/xf-day-lines 13 (map #(str/split % #",")))
         (first)
         (remove #{"x"})
         (map #(Integer/parseInt %))
         (map #(id+wait+departure % target))
         (sort-by second)
         (first)
         (take 2)
         (apply *))))
