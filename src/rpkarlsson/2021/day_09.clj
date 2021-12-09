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

(defn get-coord
  [grid [x y]]
  (nth (nth grid x nil) y nil))

(defn collect-low-point-coords
  [grid coords]
  (->> coords
       (reduce (fn [found next-coord]
              (let [neighbours (->> (neighbouring-coords next-coord)
                                    (map #(get-coord grid %))
                                    (remove nil?))
                    next-coord-value (get-coord grid next-coord)]
                (if (every? #(< next-coord-value %) neighbours)
                  (conj found next-coord)
                  found)))
            [])))

(def grid
  (with-open [r (io/reader (io/resource "2021/day_09.txt"))]
    (->> #_(str/split sample #"\n")
         (line-seq r)
         (map (partial map str))
         (mapv (partial map #(Integer/parseInt %))))))

(defn all-coords
  [grid]
  (for [x (range (count grid))
        y (range (count (first grid)))]
    [x y]))

(defn part-1
  []
  (->> (all-coords grid)
       (collect-low-point-coords grid)
       (map #(get-coord grid %))
       (map inc)
       (reduce +)))

(defn find-neighbours [grid low-points]
  (->> low-points
       (map (fn [point]
              (loop [found-points #{}
                     coords-to-check [point]]
                (if (empty? coords-to-check)
                  found-points
                  (let [next-coord (first coords-to-check)
                        point-value (get-coord grid next-coord)]
                    (if (or (nil? point-value)
                            (= 9 point-value)
                            (contains? found-points next-coord))
                      (recur found-points (rest coords-to-check))
                      (recur (conj found-points next-coord)
                             (rest
                              (concat
                               (into [] coords-to-check)
                               (neighbouring-coords next-coord))))))))))))


(defn part-2 []
  (->> (all-coords grid)
       (collect-low-point-coords grid)
       (find-neighbours grid)
       (sort-by count)
       (reverse)
       (take 3)
       (map count)
       (reduce *)))

(comment
  (part-1)
  (part-2)

  ,)
