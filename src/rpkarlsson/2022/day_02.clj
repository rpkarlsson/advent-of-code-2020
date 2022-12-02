(ns rpkarlsson.2022.day-02
  (:require [clojure.string :as str]))

(defn mmap
  [f xs]
  (map #(map f %) xs))

(def char->shape
  {\A :rock
   \X :rock
   \B :paper
   \Y :paper
   \C :scissors
   \Z :scissors})

(def play->score
  {[:rock :rock] 4
   [:rock :scissors] 7
   [:rock :paper] 1
   [:paper :paper] 5
   [:paper :rock] 8
   [:paper :scissors] 2
   [:scissors :scissors] 6
   [:scissors :rock] 3
   [:scissors :paper] 9})

(def sample
  "A Y
B X
C Z")

(defn solve
  [s]
  (->> (str/split (str/replace s " " "") #"\n")
       (mmap char->shape)
       (map (juxt second first))
       (map play->score)
       (reduce +)))

#_(solve sample)

(defn part-1
  []
  (-> "resources/2022/day_02.txt" slurp solve))

(def char->outcome
  {\Y :draw
   \X :lose
   \Z :win})

(def play+outcome->score
  {[:rock :draw] 4
   [:rock :win] 8
   [:rock :lose] 3
   [:paper :draw] 5
   [:paper :win] 9
   [:paper :lose] 1
   [:scissors :draw] 6
   [:scissors :lose] 2
   [:scissors :win] 7})

(defn solve-2
  [s]
  (->> (str/split (str/replace s " " "") #"\n")
       (map (juxt (comp char->shape first) (comp char->outcome second)))
       (map play+outcome->score)
       (reduce +)))

#_(solve-2 sample)

(defn part-2
  []
  (-> "resources/2022/day_02.txt" slurp solve-2))
