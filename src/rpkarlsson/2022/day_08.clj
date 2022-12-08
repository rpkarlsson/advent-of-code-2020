(ns rpkarlsson.2022.day-08
  (:require [clojure.string :as str]
            [rpkarlsson.utils :as u]))

(def sample
  "30373
25512
65332
33549
35390")

(defn all-coords-in-grid
  [g]
  (for [x (range (count g))
        y (range (count (first g)))]
    [x y]))

(defn get-coord [grid [x y]]
  (get (get grid x) y))

(defn visible-in-line? [value [before after]]
  (or (every? #(< % value) (butlast before))
      (every? #(< % value) after)))

(defn visible?
  [grid [x y :as coord]]
  (let [value (get-coord grid coord)]
    (or (visible-in-line?
         value
         (split-at (inc y) (get grid x)))
        (visible-in-line?
         value
         (split-at (inc x) (get (apply mapv vector grid) y))))))

(defn solve
  [input]
  (let [grid (->> input str/split-lines (u/mmap (comp parse-long str)) (map vec) vec)]
    (->> (all-coords-in-grid grid)
         (map (partial visible? grid))
         (filter true?)
         count)))

#_(solve sample)
#_(->> "resources/2022/day_08.txt" slurp solve)

(defn take-upto [n coll]
  (transduce
   (halt-when #(<= n %) conj)
   conj coll))

(defn viewing-distance-in-selection [value [before after]]
  [(->> (butlast before)
        reverse
        (take-upto value)
        (count))
   (->> after
        (take-upto value)
        (count))])

(defn viewing-distance
  [grid [x y :as coord]]
  (let [value (get-coord grid coord)]
    (vec (concat (viewing-distance-in-selection
                  value
                  (split-at (inc y) (get grid x)))
                 (viewing-distance-in-selection
                  value
                  (split-at (inc x) (get (apply mapv vector grid) y)))))))

(defn solve-2
  [input]
  (let [grid (->> input str/split-lines (u/mmap (comp parse-long str)) (map vec) vec)]
    (->> (all-coords-in-grid grid)
         (map (partial viewing-distance? grid))
         (map #(reduce * %))
         (sort)
         (reverse)
         (first))))

#_(solve-2 sample)
#_(->> "resources/2022/day_08.txt" slurp solve-2)
