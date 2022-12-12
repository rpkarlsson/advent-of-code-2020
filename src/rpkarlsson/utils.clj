(ns rpkarlsson.utils
  (:require [clojure.string :as str]))

(defn mmap
  ([f]
   (map (partial map f)))
  ([f coll]
   (map (partial map f) coll)))

(defn mcatmap
  ([f]
   (mapcat (partial map f)))
  ([f coll]
   (map (partial map f) coll)))

(defn generate-coords-in-grid
  "Given a grid of size `x` and `y` generates all coords in that grid."
  [x y]
  (->> (for [x' (range 0 x)
            y' (range 0 y)]
        [x' y'])
       (map vec)
       vec))

(defn make-grid [rows columns value]
  (->> (range (* rows columns))
       (map (constantly value))
       (partition columns)
       (map vec)
       vec))

(defn str->grid
  [s]
  (->> (str/split-lines s)
       (map vec)
       vec))

(defn adjacent-coords
  [[x y :as coord]]
  (for [x1 (range -1 2)
        y1 (range -1 2)
        :when (not= coord  [(+ x x1) (+ y y1)])]
    [(+ x x1) (+ y y1)]))

(defn neighbour-coords
  [[x y]]
  [[(dec x) y]
   [x (dec y)]
   [x (inc y)]
   [(inc x) y]])
