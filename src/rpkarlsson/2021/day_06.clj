(ns rpkarlsson.2021.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def birthing-rate 6)

(def new-fish-birthing-rate 2)

(def sample  [3,4,3,1,2])

(defn run-loop
  [state days]
  (if (= days 0)
    state
    (recur
     {0 (get state 1 0)
      1 (get state 2 0)
      2 (get state 3 0)
      3 (get state 4 0)
      4 (get state 5 0)
      5 (get state 6 0)
      6 (+ (get state 0 0) (get state 7 0) )
      7 (get state 8 0)
      8 (get state 0 0)}
     (dec days))))

(defn run
  [days]
  (let [input (->> (str/split (slurp (io/resource "2021/day_06.txt")) #",")
                   (map str/trim)
                   (map #(Integer/parseInt %))
                   (frequencies))]
    (->> (run-loop input days)
         (map second)
         (reduce +))))

(defn part-1 [] (run 80))
(defn part-2 [] (run 256))

(comment
  (part-1)
  (part-2)

  ,)
