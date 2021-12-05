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
  (let [xlength (Math/abs (- x2 x1))
        xstart (Math/min x1 x2)
        ylength (Math/abs (- y2 y1))
        ystart (Math/min y1 y2)]
    (when-not (and (zero? xlength)
                   (zero? ylength))
      (map (fn [x y] [x y])
           (if (zero? xlength)
             (repeat x1)
             (range xstart (+ 1 xstart xlength)))
           (if (zero? ylength)
             (repeat y1)
             (range ystart (+ 1 ystart ylength)))))))

(defn expand-lines
  [[[x1 y1] [x2 y2]]]
  (let [x1int (Integer/parseInt x1)
        x2int (Integer/parseInt x2)
        y1int (Integer/parseInt y1)
        y2int (Integer/parseInt y2)]
    (when (or (= x1int x2int)
              (= y1int y2int))
      (expand-line [x1int y1int] [x2int y2int]))))

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
  [[[x1 y1] [x2 y2]]]
(let [x1int (Integer/parseInt x1)
        x2int (Integer/parseInt x2)
        xlength (Math/abs (- x2int x1int))
        y1int (Integer/parseInt y1)
        y2int (Integer/parseInt y2)
        ylength (Math/abs (- y2int y1int))]
  (if-not (or (= x1int x2int)
          (= y1int y2int))
    (expand-diagonal [x1int y1int] [x2int y2int])
    (when-not (and (zero? xlength)
                   (zero? ylength))
      (expand-line [x1int y1int] [x2int y2int])))))

(defn part-2
  []
  (with-open [f (io/reader (io/resource "2021/day_05.txt"))]
    (->> (into [] parse-lines (line-seq f))
         (mapcat expand-lines-2)
         (frequencies)
         (map second)
         (filter #(<= 2 %))
         (count))))
