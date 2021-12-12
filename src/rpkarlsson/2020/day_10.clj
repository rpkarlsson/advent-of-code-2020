(ns rpkarlsson.2020.day-10
  (:require [rpkarlsson.util.file :as file]))

(defn find-connectable
  [rating adapters]
  (->> adapters
       (map-indexed (fn [index x] [index x]))
       (filter (fn [[_ x]] (<= (- x 3) rating x)))))

(defn drop-nth [n coll]
  (concat
   (take n coll)
   (drop (inc n) coll)))

(defn find-differences
  [adapters]
  (loop [adapters' (conj (into [] (sort adapters)) (+ (apply max adapters) 3))
         target 0
         differences []]
    (cond
      (empty? adapters') differences
      :else
      (if-let [[index value] (first (find-connectable target adapters'))]
        (recur (drop-nth index adapters')
               value
               (conj differences (- value target)))
        differences))))

(defn part-1
  []
  (let [freq (->> (file/xf-day-lines 10 (map #(Integer/parseInt %)))
                  (find-differences)
                  (frequencies))]
    (* (get freq 1) (get freq 3))))
