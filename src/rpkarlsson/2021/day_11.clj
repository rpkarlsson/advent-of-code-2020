(ns rpkarlsson.2021.day-11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [rpkarlsson.utils :refer :all]))

(def parse-input-xform
  (comp
   (mmap str)
   (mmap parse-long)))

(def sample
  (into [] parse-input-xform
        (str/split
         "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526" #"\n")))

(def input
  (with-open [f (io/reader (io/resource "2021/day_11.txt"))]
    (into [] parse-input-xform (line-seq f))))

(defn flashing? [x]
  (and (number? x) (< 9 x)))

(defn will-flash?
  [grid]
  (->> grid
       (mapcat identity)
       (remove keyword?)
       (some flashing?)))

(defn flash
  [row]
  (->> row
       (map #(if (flashing? %) :flash %))))

(defn all-coords
  [grid]
  (for [x (range (count grid))
        y (range (count (first grid)))]
    [x y]))

(def all-coords-seq
  (all-coords input))

(defn get-coord
  [grid [x y]]
  (nth (nth grid x nil) y nil))

(defn neighbour-coords
  [[x y :as coord]]
  (for [x1 (range -1 2)
        y1 (range -1 2)
        :when (not= coord  [(+ x x1) (+ y y1)])]
    [(+ x x1) (+ y y1)]))

(def valid-non-flashing? (every-pred (complement nil?) (complement #{:flash})))

(defn inc-neighbour
  [g coord]
  (->> (neighbour-coords coord)
       (reduce (fn [new-grid neighbour]
                 (if (valid-non-flashing? (get-coord g neighbour))
                   (update-in new-grid neighbour inc)
                   new-grid))
               (mapv #(into [] %) g))))

(defn inc-neighbours
  [grid]
  (let [next-grid (->> all-coords-seq
                       (filter #(flashing? (get-coord grid %)))
                       (reduce inc-neighbour (map flash grid)))]
    (if (not= grid next-grid)
      (recur next-grid)
      (->> next-grid
           (mmap #(if (= :flash %) 0 %))))))

(defn step
  [grid]
  (let [incremented-grid (mapv (partial map inc)
                               grid)]

    (loop [iteration incremented-grid]
      (if-not (will-flash? iteration)
        iteration
        (recur (inc-neighbours iteration))))))

(defn part-1
  []
  (->> (loop [steps (list input)]
         (if (< 100 (count steps))
           steps
           (recur (conj steps (step (first steps))))))
       (mapcat (partial mapcat identity))
       (filter zero?)
       (count)))

(defn part-2
  []
  (->> (loop [steps (list (step input))]
         (if (every? zero? (mapcat identity (first steps)))
           steps
           (recur (conj steps (step (first steps))))))
       (count)))

(comment
  (part-1)
  (part-2)

  ,)
