(ns rpkarlsson.2022.day-06)

(defn solve-1
  []
  (->> "resources/2022/day_06.txt"
       slurp
       (partition-all 4 1)
       (reduce (fn [idx next]
                 (if (= 4 (count (set next)))
                   (reduced idx)
                   (inc idx)))
               4)))

(defn solve-2
  []
  (->> "resources/2022/day_06.txt"
       slurp
       (partition-all 14 1)
       (reduce (fn [idx next]
                 (if (= 14 (count (set next)))
                   (reduced idx)
                   (inc idx)))
               14)))
