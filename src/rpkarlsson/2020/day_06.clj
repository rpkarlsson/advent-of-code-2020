(ns rpkarlsson.2020.day-06
  (:require [clojure.string :as str]
            [rpkarlsson.util.file :as file]
            [clojure.set :as set]))

(def by-group-xf (comp
   (partition-by str/blank?)
   (remove (comp str/blank? first))))

(def distinct-group-answer-set-xf
  (comp by-group-xf (map #(mapcat identity %)) (map #(into #{} %))))

(defn total-count [xs]
  (reduce + (map count xs)))

(defn part-1 []
  (total-count (file/xf-day-lines 6 distinct-group-answer-set-xf)))

(def questions (into #{} "abcdefghijklmnopqrstuvwxyz"))

(def group-answer-sets-xf
  (comp by-group-xf (map (partial map #(into #{} %)))))

(defn part-2
  []
  (->> (file/xf-day-lines 6 group-answer-sets-xf)
       (map #(apply (partial set/intersection questions) %))
       (total-count)))

(comment
  (part-1)
  (part-2))
