(ns rpkarlsson.2022.day-06)

(defn solve
  [repetitions]
  (->> "resources/2022/day_06.txt"
       slurp
       (partition-all repetitions 1)
       (reduce (fn [idx next]
                 (if (apply distinct? next)
                   (reduced idx)
                   (inc idx)))
               repetitions)))
;; Part 1
#_(solve 4)

;; Part 2
#_(solve 14)
