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
     (->> state
          (mapcat (fn [n]
                    (if (zero? n)
                      [birthing-rate (+ birthing-rate new-fish-birthing-rate)]
                      [(dec n)]))))
     (dec days))))

(defn part-1
  []
(let [input (->> (str/split (slurp (io/resource "2021/day_06.txt")) #",")
                   (map str/trim)
                   (map #(Integer/parseInt %)))]
    (->> (run-loop input 80)
         (count))))

(comment
  (part-1)

  ,)
