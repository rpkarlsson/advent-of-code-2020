(ns rpkarlsson.2022.day-01
  (:require [clojure.string :as str]
            [rpkarlsson.utils :as u]))

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

(def summarize-per-elf-xf
  (comp
   (map str/split-lines)
   (u/mmap #(Integer/parseInt %))
   (map #(reduce + %))
   (map-indexed vector)
   (map (juxt (comp inc first) second))))

(defn sum-by-elf
  [s]
  (->> (str/split s #"\n\n")
       (into [] summarize-per-elf-xf)
       (sort-by second)
       (reverse)))

#_(ffirst (sum-by-elf sample))

(defn part-1
  []
  (-> "resources/2022/day_01.txt"
      slurp
      sum-by-elf
      first))

(defn part-2
  []
  (->> "resources/2022/day_01.txt"
       slurp
       sum-by-elf
       (transduce (comp (take 3)
                        (map second))
                  +)))
