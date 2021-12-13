(ns rpkarlsson.2021.day-13
  (:require
   [clojure.string :as str]
   [clojure.java.math :as math]
   [rpkarlsson.utils :refer :all]
   [clojure.java.io :as io]))

(def sample
  "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defn parse-dots
  "Tuple of col row"
  [s]
  (->> s
       (str/split-lines)
       (map #(str/split % #","))
       (mmap parse-long)))

(defn parse-folds
  [s]
  (->> s
       (str/split-lines)
       (map #(str/replace-first % "fold along " ""))
       (map #(str/split % #"="))))

(defn split-at-row
  [n grid]
  (let [sorted-grid (sort-by first grid)]
    [(take-while (comp (fn [i](<= i n)) first) sorted-grid)
     (drop-while (comp (fn [i](<= i n)) first) sorted-grid)]))

(defn fold-horizontal
  [n grid]
  (let [[a b] (split-at-row 7 grid)]
    (group-by second
              (concat
               (mapcat second a)
               (->> (update-keys b #(math/abs (- % (* 2 n))))
                    (mapcat (fn [[k v]]
                              (map (fn [[c r]] [c k]) v))))))))

(defn fold-vertical
  [n grid]
  (let [grid-by-column (->> grid
                            (vals)
                            (mapcat identity)
                            (group-by first))
        [a b] (split-at-row n grid-by-column)]
    (group-by second
              (concat
               (mapcat second a)
               (->> (update-keys b #(math/abs (- (* 2 n) %)))
                    (mapcat (fn [[k v]]
                              (map (fn [[_ r]] [k r]) v))))))))

(def input
  (->> (slurp (io/resource "2021/day_13.txt"))))

(defn part-1
  []
  (let [[d f] (str/split input #_sample #"\n\n")
        dots (parse-dots d)]
    (->> (loop [grid (->> dots
                          (group-by second))
                [[fold-op fold-nth] & folds] (take 1 (parse-folds f))]
           (if (#{"x"} fold-op)
             (fold-vertical (parse-long fold-nth) grid )
             (fold-horizontal (parse-long fold-nth) grid)))
         (mapcat (comp #(into #{} %) second))
         (count))))

(comment
  (part-1)

  ,)
