(ns rpkarlsson.2021.day-11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(def sample
  (->>  (str/split "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526" #"\n")
        (map (partial map str))
        (map (partial map parse-long))))

(def input
  (with-open [f (io/reader (io/resource "2021/day_11.txt"))]
    (->> (line-seq f)
         (map (partial map str))
         (mapv (partial map parse-long)))))

(defn will-flash?
  [grid]
  (->> grid
       (mapcat identity)
       (remove keyword?)
       (some #(< 9 %))))

(defn flash
  [row]
  (->> row
       (map #(if (and (number? %)(< 9 %)) :flash %))))

(defn all-coords
  [grid]
  (for [x (range (count grid))
        y (range (count (first grid)))]
    [x y]))

(defn get-coord
  [grid [x y]]
  (nth (nth grid x nil) y nil))

(defn neighbour-coords
  [[x y]]
  (vector
   [(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
   [x (dec y)]                   [x (inc y)]
   [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]))

(defn inc-neighbour
  [g coord]
  (->> (neighbour-coords coord)
       (reduce (fn [new-grid neighbour]
                 (let [v (get-coord g neighbour)]
                   (if (or (nil? v)
                           (= :flash v))
                     new-grid
                     (update-in new-grid neighbour inc))))
               (mapv #(into [] %) g))
       ))

(defn find-flashing
  [grid]
  (->> (all-coords grid)
       (reduce (fn [coll next-coord]
                 (if (and (number? (get-coord grid next-coord))
                          (< 9  (get-coord grid next-coord)))
                   (conj coll next-coord)
                   coll))
               [])))

(def flashes (atom 0))

(defn inc-neighbours
  [grid]
  (let [flashing-coords
        (->> (find-flashing grid))
        next-grid
        (->> flashing-coords

             (reduce inc-neighbour (mapv flash grid)))]
    (if (not= grid next-grid)
      (recur next-grid)
      (->> next-grid
           (mapv (partial map #(if (= :flash %) (do (swap! flashes inc) 0) %)))))))

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
  (->> (range 100)
       (reduce (fn [result _]
                 (step result))
               input))
  (let [result @flashes]
    (reset! flashes 0)
    result))


(comment
  (part-1)

  ,)
