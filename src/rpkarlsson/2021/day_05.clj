(ns rpkarlsson.2021.day-05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
")

(def parse-lines
  (comp (map #(str/split % #" -> "))
        (map (partial map #(str/split % #",")))))

(def parsed-sample
  (->> (str/split sample #"\n")
       (into [] parse-lines)))

(defn expand-line
  [[x1 y1] [x2 y2]]
  (for [x (range (Math/min x1 x2) (inc (Math/max x1 x2)))
        y (range (Math/min y1 y2) (inc (Math/max y1 y2)))]
    [x y]))

(defn expand-lines
  [points]
  (let [[pos1 pos2] (->> points
                         (map (partial map #(Integer/parseInt %))))]
    (when (or (= (first pos1) (first pos2))
              (= (second pos1) (second pos2)))
      (expand-line pos1 pos2))))

(defn part-1
  []
  (with-open [f (io/reader (io/resource "2021/day_05.txt"))]
    (->> (into [] parse-lines (line-seq f))
         (mapcat expand-lines)
         (frequencies)
         (map second)
         (filter #(<= 2 %))
         (count))))

(defn expand-diagonal
  [a b]
  (let [[pos1 pos2] (sort-by first [a b])
        ydir (if (< 0 (- (second pos2) (second pos1)))
               inc dec)]
    (loop [p [pos1]]
      (let [l (last p)]
        (if (not= l pos2)
          (recur (conj p [(inc (first l)) (ydir (second l))]))
          p)))))

(defn expand-lines-2
  [points]
  (let [[pos1 pos2] (->> points
                         (map (partial map #(Integer/parseInt %))))]
    (if-not (or (= (first pos1) (first pos2))
                (= (second pos1) (second pos2)))
      (expand-diagonal pos1 pos2)
      (expand-line pos1 pos2))))

(defn part-2
  []
  (with-open [f (io/reader (io/resource "2021/day_05.txt"))]
    (->> (into [] parse-lines (line-seq f))
         (mapcat expand-lines-2)
         (frequencies)
         (map second)
         (filter #(<= 2 %))
         (count))))

(comment
  (part-1)
  (part-2)

  ,)
