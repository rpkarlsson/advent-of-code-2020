(ns rpkarlsson.2022.day-01
  (:require [clojure.string :as str]))

(def sample
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn mmap
  [f xs]
  (map #(map f %) xs))

(defn sum-by-elf
  [s]
  (->> (str/split s #"\n\n")
       (map str/split-lines)
       (mmap #(Integer/parseInt %))
       (map #(reduce + %))
       (map-indexed vector)
       (map (juxt (comp inc first) second))
       (sort-by second)
       (reverse)))

#_(ffirst (sum-by-elf sample))

(defn part-1
  []
  (first (sum-by-elf (slurp "resources/2022/day_01.txt"))))

(defn part-2
  []
  (->> "resources/2022/day_01.txt"
       slurp
       sum-by-elf
       (take 3)
       (map second)
       (reduce +)))
