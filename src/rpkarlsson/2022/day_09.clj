(ns rpkarlsson.2022.day-09
  (:require [clojure.string :as str]
            [rpkarlsson.utils :as u]))

(def sample
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")


(defn ->directions [[direction times]]
  (take times (repeat direction)))

(defn parse [s]
  (->> s
       str/split-lines
       (map #(str/split % #" "))
       (map (juxt first (comp parse-long second)))
       (mapcat ->directions)))


(defn move
  [[x y] move]
  (case move
    "R" [x (inc y)]
    "L" [x (dec y)]
    "U" [(dec x) y]
    "D" [(inc x) y]))

(defn adjacent? [[x y] tail]
  (let [n (for [x' (range (dec x) (+ x 2))
                y' (range (dec y) (+ y 2))]
            [x' y'])]
    (contains? (set n) tail)))

(defn in-row-or-col
  [[x y] [a b]]
  (or (= x a) (= y b)))

(defn run [input]
  (loop [head [5 0]
         tail [5 0]
         visited #{[5 0]}
         moves (parse input)]
    (if (empty? moves)
      visited
      (let [head' (move head (first moves))
            tail' (cond
                    (adjacent? head' tail) tail
                    (in-row-or-col head tail) (move tail (first moves))
                    :else head)]
        (recur head'
               tail'
               (conj visited tail')
               (rest moves))))))

#_(-> sample run count)
#_(->> "resources/2022/day_09.txt" slurp run count)
