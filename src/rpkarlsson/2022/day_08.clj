(ns rpkarlsson.2022.day-08
  (:require [clojure.string :as str]
            [rpkarlsson.utils :as u]))

(defn neighbour-coords
  [[x y]]
  [[(dec x) y]
   [x (dec y)]
   [x (inc y)]
   [(inc x) y]])

(def sample
  "30373
25512
65332
33549
35390")

(defn find-visible
  [])

(defn all-coords-in-grid
  [g]
  (for [x (range (count g))
        y (range (count (first g)))]
    [x y]))

(defn get-coord [grid [x y]]
  (get (get grid x) y))

(apply mapv vector
       [[1 2 3 4 5]
        [1 2 3 4 5]
        [1 2 3 4 5]])

(defn visible-in-row? [grid [x y :as coord]]
  (let [value (get-coord grid coord)
        [before after] (split-at (inc y) (get grid x))]
    (or (every? #(< % value) (butlast before))
        (every? #(< % value)  after))))

(defn visible-in-column? [grid [x y :as coord]]
  (let [value (get-coord grid coord)
        [before after] (split-at (inc x) (get (apply mapv vector grid) y))]
    (or (every? #(< % value) (butlast before))
        (every? #(< % value)  after))))

(defn visible?
  [grid [x y :as coord]]
  (if (neg? (min x y))
    true
    (or (visible-in-row? grid coord)
        (visible-in-column? grid coord))))

(let [grid (->> sample str/split-lines (u/mmap (comp parse-long str)) (map vec) vec)]
  (->> (all-coords-in-grid grid)
       (map (partial visible? grid))
       (filter true?)
       count))
