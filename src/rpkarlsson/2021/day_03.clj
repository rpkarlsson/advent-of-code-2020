(ns rpkarlsson.2021.day-03
  (:require [clojure.java.io :as io]))

(def sample
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(def binary->decimal-map [1 2 4 8 16 32 64 128 256 512 1024 2048])

(defn binary->decimal [s]
  (->> s
       (reverse)
       (map (fn [n c] (if (= c \1) n 0)) binary->decimal-map)
       (reduce +)))

(defn step-1
  []
  (with-open [f (io/reader (io/resource "2021/day_03.txt"))]
    (let [series (->> f
                      (line-seq)
                      (apply map str)
                      (map frequencies)
                      (map #(sort-by second %)))
          gamma (->> series
                     (map reverse)
                     (map ffirst)
                     (binary->decimal))
          epsilon (->> series (map ffirst) (binary->decimal))]
      (* gamma epsilon))))

(defn find-frequency
  [lines f]
  (->> lines
       (apply map str)
       (map frequencies)
       (map f)))

(defn find-most-frequent
  [lines]
  (find-frequency lines
                  #(if (= 1 (count %))
                     %
                     (if (<= (get % \0) (get % \1))
                       (select-keys % [\1])
                       (select-keys % [\0])))))

(defn find-most-infrequent
  [lines]
  (find-frequency lines
                  #(if (= 1 (count %))
                     %
                     (if (<= (get % \0) (get % \1))
                       (select-keys % [\0])
                       (select-keys % [\1])))))

(defn find-last
  [lines f]
  (reduce
   (fn [coll index]
     (let [[c _] (first (nth (->> coll f) index))]
       (if (= 1 (count coll))
         (reduced (first coll))
         (->> coll (filter #(= c (nth % index)))))))
   lines
   (range (count (first lines)))))

(defn step-2
  []
  (with-open [f (io/reader (io/resource "2021/day_03.txt"))]
    (let [lines (line-seq f)
          ogr (find-last lines find-most-frequent)
          co2sr (find-last lines find-most-infrequent)]
      (* (binary->decimal co2sr)
         (binary->decimal ogr)))))

(comment

  (step-1)
  (step-2)

  ,)
