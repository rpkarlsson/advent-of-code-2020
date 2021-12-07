(ns rpkarlsson.2021.day-07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample "16,1,2,0,4,2,7,1,2,14")

(def input-xf
  (comp
   (mapcat #(str/split % #","))
   (map #(str/trim %))
   (map #(Integer/parseInt %))))

(defn part-1
  []
  (let [data (into [] input-xf [#_sample (slurp (io/resource "2021/day_07.txt"))])
        mean (nth (sort data) (/ (int (count data)) 2))]
    (->> data
         (map #(- % mean))
         (map #(Math/abs %))
         (reduce +))))

(defn part-2
  []
  (let [data (into [] input-xf [#_sample (slurp (io/resource "2021/day_07.txt"))])]
    (apply min
           (->> (range (apply min data) (apply max data))
                (map (fn [pos]
                       (->> data
                            (map #(- % pos))
                            (map #(Math/abs %))
                            (map #(+ % (reduce + (range %))))
                            (reduce +))))))))

(comment
  (part-1)
  (part-2)

  ,)
