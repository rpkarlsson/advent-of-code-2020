(ns rpkarlsson.day-01
  (:require
   [criterium.core :as c]
   [rpkarlsson.util.file :as file]))

(def input (delay (file/xf-day-lines 1 (map #(Integer/parseInt %)))))

(defn solve-1
  []
  (loop [xs @input]
    (if (empty? xs) nil
        (let [frst (first xs)
              rst (rest xs)]
          (or (reduce (fn [_ i]
                        (if (= 2020 (+ frst i))
                          (reduced (* frst i))
                          nil))
                      rst)
              (recur rst))))))

(defn solve-2
  []
  (let [xs @input]
    (->> (for [x xs
               y (drop 1 xs)
               z (drop 2 xs)
               :when (= 2020 (+ x y z))]
           (* x y z ))
         (remove nil?)
         (first))))

(defn solve-1-redone
  []
  (let [xs @input]
    (->> (for [x xs
               y (rest xs)
               :when (= 2020 (+ x y))]
           (* x y))
         (remove nil?)
         (first))))

(comment
  (c/bench (solve-1))
  (c/bench (solve-1-redone))
  (c/bench (solve-2)))
