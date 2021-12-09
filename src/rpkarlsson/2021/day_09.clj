(ns rpkarlsson.2021.day-09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(def sample
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn neighbouring-coords
  [[x y]]
  [[(dec x) y]  [(inc x) y]
   [x (dec y)]  [x (inc y)]])

(defn collect-low-points
  [grid coords]
  (->> coords
       (reduce (fn [found next-coord]
              (let [neighbours (->> (neighbouring-coords next-coord)
                                    (map #(nth (nth grid (first %) []) (second %) nil))
                                    (remove nil?)
                                    )
                    next-coord-value (nth (nth grid (first next-coord)) (second next-coord))]
                (if (every? #(< next-coord-value %) neighbours)
                  (conj found next-coord-value)
                  found)))
            [])))

(def part-1
  []
  (let [grid (with-open [r (io/reader (io/resource "2021/day_09.txt"))]
               (->> #_(str/split sample #"\n")
                    (line-seq r)
                    (map (partial map str))
                    (mapv (partial map #(Integer/parseInt %)))))
        coords (for [x (range (count grid))
                     y (range (count (first grid)))]
                 [x y])]
    (->> coords
         (collect-low-points grid)
         (map inc)
         (reduce +))))
