(ns rpkarlsson.2021.day-01
  (:require [clojure.java.io :as io]))

(def sample
  [199 200 208 210 200 207 240 269 260 263])

(defn count-increases
  [{:keys [prev] :as acc} next]
  (if (< prev next)
    (-> acc
        (update :increase inc)
        (assoc :prev next))
    (assoc acc :prev next)))

(comment
  (update {:a 0} :a inc)
  (count-increases {:prev 1 :increase 0} 2)

  (reduce count-increases {:increase -1} sample)

  (->> (slurp "resources/2021/day_01.txt"))

  (with-open [f (io/reader (io/resource "2021/day_01.txt"))]
    (->> (line-seq f)
         (map #(Integer/parseInt %))
         (reduce count-increases {:prev 1 :increase -1} )))

  (->> sample
       (partition-all 3 1)
       (map #(reduce + %))
       (reduce count-increases {:prev 1 :increase -1}))

(with-open [f (io/reader (io/resource "2021/day_01.txt"))]
    (->> (line-seq f)
         (map #(Integer/parseInt %))
         (partition-all 3 1)
         (map #(reduce + %))
         (reduce count-increases {:prev 1 :increase -1} )))


  ,)
