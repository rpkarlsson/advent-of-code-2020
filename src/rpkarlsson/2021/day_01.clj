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

(defn part-1
  []
  (with-open [f (io/reader (io/resource "2021/day_01.txt"))]
    (->> (line-seq f)
         (map #(Integer/parseInt %))
         (reduce count-increases {:prev 1 :increase -1} ))))


(defn part-2
  []
  (with-open [f (io/reader (io/resource "2021/day_01.txt"))]
    (->> (line-seq f)
         (map #(Integer/parseInt %))
         (partition-all 3 1)
         (map #(reduce + %))
         (reduce count-increases {:prev 1 :increase -1} ))))

(comment
  (count-increases {:prev 1 :increase 0} 2)

  (->> sample
       (partition-all 3 1)
       (map #(reduce + %))
       (reduce count-increases {:prev 1 :increase -1}))

  (part-1)
  (part-2)

  ,)
