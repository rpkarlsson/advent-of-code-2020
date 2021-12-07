(ns rpkarlsson.2021.day-07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample "16,1,2,0,4,2,7,1,2,14")

(def input-xf
  (comp
   (mapcat #(str/split % #","))
   (map #(str/trim %))
   (map #(Integer/parseInt %))))

(let [data (into [] input-xf [sample #_(slurp (io/resource "2021/day_07.txt"))])
      mean (nth (sort data) (/ (int (count data)) 2))]
  (->> data
       (map #(- % mean))
       (map #(Math/abs %))
       (map (fn [n] (reduce + (range (inc n)))))
       (reduce +)))
