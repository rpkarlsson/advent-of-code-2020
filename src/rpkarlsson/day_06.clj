(ns rpkarlsson.day-06
  (:require [clojure.string :as str]
            [rpkarlsson.util.file :as file]
            [clojure.set :as set]))

(def by-group-xf
  (comp
   (partition-by str/blank?)
   (remove (comp str/blank? first))))

(def distinct-group-answer-set-xf
  (comp
   by-group-xf
   (map #(mapcat identity %))
   (map #(into #{} %))))

(defn part-1
  []
  (->> (file/xf-day-lines 6 distinct-group-answer-set-xf)
       (map count)
       (reduce +)))

(def questions
  (->> "abcdefghijklmnopqrstuvwxyz"
       (into #{})))

(def group-answer-sets-xf
  (comp
   by-group-xf
   (map (partial map #(into #{} %)))))

(defn part-2
  []
  (->> (file/xf-day-lines 6 group-answer-sets-xf)
       (map #(apply (partial set/intersection questions) %))
       (map count)
       (reduce +)))
